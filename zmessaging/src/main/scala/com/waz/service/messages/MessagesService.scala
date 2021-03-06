/*
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.service.messages

import android.util.Base64
import com.waz.ZLog._
import com.waz.api.Message.{Status, Type}
import com.waz.api.{ErrorResponse, Message, Verification}
import com.waz.content.{LikingsStorage, Mime}
import com.waz.model.AssetStatus.UploadCancelled
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.Asset.Original
import com.waz.model.GenericContent._
import com.waz.model.{IdentityChangedError, MessageId, _}
import com.waz.service._
import com.waz.service.assets.AssetService
import com.waz.service.conversation.{ConversationsContentUpdater, ConversationsService}
import com.waz.service.otr.VerificationStateUpdater
import com.waz.service.otr.VerificationStateUpdater.{ClientAdded, ClientUnverified, MemberAdded, VerificationChange}
import com.waz.sync.SyncServiceHandle
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.RichFuture.traverseSequential
import com.waz.utils._
import com.waz.utils.crypto.AESUtils
import com.waz.utils.events.{EventContext, Signal}
import org.threeten.bp.Instant

import scala.collection.breakOut
import scala.concurrent.Future
import scala.concurrent.Future.{successful, traverse}
import scala.util.Success

class MessagesService(selfUserId: UserId, val content: MessagesContentUpdater, assets: AssetService, users: UserService, convs: ConversationsContentUpdater,
    likings: LikingsStorage, network: NetworkModeService, sync: SyncServiceHandle, verificationUpdater: VerificationStateUpdater) {
  import Threading.Implicits.Background
  private implicit val logTag: LogTag = logTagFor[MessagesService]
  private implicit val ec = EventContext.Global

  import assets._
  import content._
  import convs._
  import users._

  val messageEventProcessingStage = EventScheduler.Stage[MessageEvent] { (convId, events) =>
    verbose(s"got events to process: $events")
    processConvWithRemoteId(convId, retryAsync = true) { conv =>
      verbose(s"processing events for conv: $conv, events: $events")
      processEvents(conv, events)
    }
  }

  verificationUpdater.updateProcessor = { update =>
    addMessagesAfterVerificationUpdate(update.convUpdates, update.convUsers, update.changes) map { _ => Unit }
  }

  private[service] def processEvents(conv: ConversationData, events: Seq[MessageEvent]): Future[Set[MessageData]] = {
    val filtered = events.filter(e => conv.cleared.isBefore(e.time.instant))

    val recalls = filtered collect { case GenericMessageEvent(_, _, time, from, msg @ GenericMessage(_, MsgRecall(ref))) => (msg, from, time.instant) }

    for {
      as    <- updateAssets(filtered)
      msgs  = filtered map { createMessage(conv, _) } filter (_ != MessageData.Empty)
      _     = verbose(s"messages from events: $msgs")
      res   <- content.addMessages(conv.id, msgs)
      _     <- updateLastReadFromOwnMessages(conv.id, msgs)
      _     <- deleteCancelled(as)
      _     <- Future.traverse(recalls) { case (GenericMessage(id, MsgRecall(ref)), user, time) => recallMessage(conv.id, ref, user, MessageId(id.str), time, Message.Status.SENT) }
    } yield res
  }

  private def updateAssets(events: Seq[MessageEvent]) = Future.sequence(events.collect {
    case GenericMessageEvent(_, convId, time, from, GenericMessage(id, asset: Asset)) =>
      updateAsset(AssetId(id.str), convId, asset, None, time)

    case GenericAssetEvent(_, convId, time, from, GenericMessage(assetId, asset: Asset), dataId, _) =>
      updateAsset(AssetId(assetId.str), convId, asset, Some(dataId), time)

    case GenericAssetEvent(_, convId, time, from, msg @ GenericMessage(assetId, ImageAsset(tag, width, height, origWidth, origHeight, mime, size, None, _)), dataId, data) =>
      warn(s"got otr asset event without otr key: $msg")
      val img = ImageData(tag, mime.str, width, height, origWidth, origHeight, size, Some(dataId), sent = true, data64 = data flatMap { arr => LoggedTry(Base64.encodeToString(arr, Base64.DEFAULT)).toOption })
      updateImageAsset(AssetId(assetId.str), convId, img)

    case GenericAssetEvent(_, convId, time, from, GenericMessage(assetId, ImageAsset(tag, width, height, origWidth, origHeight, mime, size, Some(key), sha)), dataId, data) =>
      val img = data.fold {
        ImageData(tag, mime.str, width, height, origWidth, origHeight, size, Some(dataId), sent = true, otrKey = Some(key), sha256 = sha)
      } { img =>
        val data64 = if (sha.forall(_.str == sha2(img))) LoggedTry(Base64.encodeToString(AESUtils.decrypt(key, img), Base64.DEFAULT)).toOption else None
        ImageData(tag, mime.str, width, height, origWidth, origHeight, size, Some(dataId), sent = true, otrKey = Some(key), sha256 = sha, data64 = data64)
      }
      updateImageAsset(AssetId(assetId.str), convId, img)
  })

  private def updateLastReadFromOwnMessages(convId: ConvId, msgs: Seq[MessageData]) =
    msgs.reverseIterator.find(_.userId == selfUserId).fold2(Future.successful(None), msg => updateConversationLastRead(convId, msg.time))

  private def deleteCancelled(as: Seq[AssetData]) = {
    val toRemove = as collect {
      case AnyAssetData(id, _, _, _, _, _, _, _, _, UploadCancelled, _) => id
    }
    if (toRemove.isEmpty) Future.successful(())
    else for {
      _ <- Future.traverse(toRemove)(id => content.messagesStorage.remove(MessageId(id.str)))
      _ <- assets.storage.remove(toRemove)
    } yield ()
  }

  private def createMessage(conv: ConversationData, event: MessageEvent) = {
    val id = MessageId(event.id)
    val convId = conv.id

    event match {
      case ConnectRequestEvent(_, _, eventId, time, from, text, recipient, name, email) =>
        MessageData(id, convId, eventId, Message.Type.CONNECT_REQUEST, from, MessageData.textContent(text), recipient = Some(recipient), email = email, name = Some(name), time = time.instant, localTime = event.localTime.instant)
      case RenameConversationEvent(_, _, eventId, time, from, name) =>
        MessageData(id, convId, eventId, Message.Type.RENAME, from, name = Some(name), time = time.instant, localTime = event.localTime.instant)
      case MemberJoinEvent(_, _, eventId, time, from, userIds) =>
        MessageData(id, convId, eventId, Message.Type.MEMBER_JOIN, from, members = userIds.toSet, time = time.instant, localTime = event.localTime.instant)
      case MemberLeaveEvent(_, _, eventId, time, from, userIds) =>
        MessageData(id, convId, eventId, Message.Type.MEMBER_LEAVE, from, members = userIds.toSet, time = time.instant, localTime = event.localTime.instant)
      case MissedCallEvent(_, _, eventId, time, from) =>
        MessageData(id, convId, eventId, Message.Type.MISSED_CALL, from, time = time.instant, localTime = event.localTime.instant)
      case _: VoiceChannelDeactivateEvent =>
        MessageData.Empty // don't add any message, interesting case is handled with MissedCallEvent extractor
      case OtrErrorEvent(_, _, time, from, IdentityChangedError(_, _)) =>
        MessageData (id, conv.id, EventId.Zero, Message.Type.OTR_IDENTITY_CHANGED, from, time = time.instant, localTime = event.localTime.instant)
      case OtrErrorEvent(_, _, time, from, otrError) =>
        MessageData (id, conv.id, EventId.Zero, Message.Type.OTR_ERROR, from, time = time.instant, localTime = event.localTime.instant)
      case GenericMessageEvent(_, _, time, from, proto @ GenericMessage(uid, msgContent)) =>
        val id = MessageId(uid.str)
        msgContent match {
          case Text(text, mentions, links) =>
            val (tpe, content) = MessageData.messageContent(text, mentions, links)
            MessageData(id, conv.id, EventId.Zero, tpe, from, content, time = time.instant, localTime = event.localTime.instant, protos = Seq(proto))
          case Knock(hotKnock) =>
            MessageData(id, conv.id, EventId.Zero, Message.Type.KNOCK, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(proto))
          case action: Liking.Action => MessageData.Empty
          case Asset(_, _, UploadCancelled) => MessageData.Empty
          case Asset(Some(Original(Mime.Video(), _, _, _, _)), _, _) =>
            MessageData(id, convId, EventId.Zero, Message.Type.VIDEO_ASSET, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(proto))
          case Asset(Some(Original(Mime.Audio(), _, _, _, _)), _, _) =>
            MessageData(id, convId, EventId.Zero, Message.Type.AUDIO_ASSET, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(proto))
          case Asset(_, _, _) =>
            MessageData(id, convId, EventId.Zero, Message.Type.ANY_ASSET, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(proto))
          case Location(_, _, _, _) =>
            MessageData(id, convId, EventId.Zero, Message.Type.LOCATION, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(proto))
          case LastRead(remoteId, timestamp) => MessageData.Empty
          case Cleared(remoteId, timestamp) => MessageData.Empty
          case MsgDeleted(_, _) => MessageData.Empty
          case MsgRecall(_) => MessageData.Empty
          case _ =>
            error(s"unexpected generic message content: $msgContent")
            // TODO: this message should be processed again after app update, maybe future app version will understand it
            MessageData(id, conv.id, EventId.Zero, Message.Type.UNKNOWN, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(proto))
        }
      case GenericAssetEvent(_, _, time, from, msg, dataId, data) =>
        msg match {
          case GenericMessage(assetId, asset @ Asset(Some(Original(Mime.Video(), _, _, _, _)), _, _)) =>
            MessageData(MessageId(assetId.str), convId, EventId.Zero, Message.Type.VIDEO_ASSET, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(msg))
          case GenericMessage(assetId, asset @ Asset(Some(Original(Mime.Audio(), _, _, _, _)), _, _)) =>
            MessageData(MessageId(assetId.str), convId, EventId.Zero, Message.Type.AUDIO_ASSET, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(msg))
          case GenericMessage(assetId, asset: Asset) =>
            MessageData(MessageId(assetId.str), convId, EventId.Zero, Message.Type.ANY_ASSET, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(msg))
          case GenericMessage(assetId, im: ImageAsset) =>
            MessageData(MessageId(assetId.str), convId, EventId.Zero, Message.Type.ASSET, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(msg))
          case _ =>
            error(s"unexpected generic asset content: $msg")
            // TODO: this message should be processed again after app update, maybe future app version will understand it
            MessageData(id, conv.id, EventId.Zero, Message.Type.UNKNOWN, from, time = time.instant, localTime = event.localTime.instant, protos = Seq(msg))
        }
      case _ =>
        warn(s"Unexpected event for addMessage: $event")
        MessageData.Empty
    }
  }

  def updateKnockToHotKnock(msgId: MessageId) = messagesStorage.update(msgId, { msg =>
    if (msg.hotKnock) msg
    else msg.copy(protos = msg.protos :+ GenericMessage(msgId, Knock(true)), localTime = Instant.now)
  }).mapSome(_._2)

  /**
   * Return last message if it's KNOCK message added by self user.
   */
  def getActiveKnockMessage(convId: ConvId, selfUserId: UserId): Future[Option[MessageData]] = messagesStorage.getLastMessage(convId) map { lastMsg =>
    lastMsg.filter(m => m.userId == selfUserId && m.msgType == Message.Type.KNOCK && !ConversationsService.knockExpired(m))
  }

  def activeKnockMessage(convId: ConvId) = messagesStorage.lastMessage(convId) flatMap {
    case Some(m) if m.msgType == Message.Type.KNOCK && !ConversationsService.knockExpired(m) =>
      Signal.future(users.getSelfUserId.map(_.flatMap { id => Some(m).filter(_.userId == id) }))
    case _ => Signal.const(Option.empty[MessageData])
  }

  def getIncomingMessages = messagesStorage.getIncomingMessages flatMap { msgs =>
    Signal.future {
      convs.storage.getAll(msgs.map(_.convId).distinct) map { cs =>
        val muted = cs.flatten.filter(_.muted).map(_.id).toSet
        msgs filterNot { msg => muted(msg.convId) }
      }
    }
  }

  def recallMessage(convId: ConvId, msgId: MessageId, userId: UserId, systemMessageId: MessageId = MessageId(), time: Instant = Instant.now(), state: Message.Status = Message.Status.PENDING) =
    content.getMessage(msgId) flatMap {
      case Some(msg) if msg.canRecall(convId, userId) =>
        content.deleteOnUserRequest(Seq(msgId)) flatMap { _ =>
          content.addMessage(MessageData(systemMessageId, convId, EventId.Zero, Message.Type.RECALLED, editTime = time max msg.time, userId = userId, state = state, protos = Seq(GenericMessage(systemMessageId, MsgRecall(msgId)))))
        }
      case _ => Future successful None
    }

  def addTextMessage(convId: ConvId, content: String, mentions: Map[UserId, String] = Map.empty): Future[MessageData] = {
    verbose(s"addTextMessage($convId, $content, $mentions)")
    val (tpe, ct) = MessageData.messageContent(content, mentions, weblinkEnabled = true)
    verbose(s"parsed content: $ct")
    val id = MessageId()
    addLocalMessage(MessageData(id, convId, EventId.Zero, tpe, selfUserId, ct, protos = Seq(GenericMessage(id, Text(content, mentions, Nil))))) // FIXME: links
  }

  def addLocationMessage(convId: ConvId, content: Location): Future[MessageData] = {
    verbose(s"addLocationMessage($convId, $content)")
    val id = MessageId()
    addLocalMessage(MessageData(id, convId, EventId.Zero, Type.LOCATION, selfUserId, protos = Seq(GenericMessage(id, content))))
  }

  def addImageMessage(convId: ConvId, assetId: AssetId, width: Int, height: Int) = {
    val id = MessageId(assetId.str)
    addLocalMessage(MessageData(id, convId, EventId.Zero, Message.Type.ASSET, selfUserId, protos = Seq(GenericMessage(id, Asset(Original(Mime("image/jpeg"), 0, None, Some(AssetMetaData.Image(Dim2(width, height), Some(ImageData.Tag.Medium)))))))))
  }

  def addAssetMessage(convId: ConvId, assetId: AssetId, mime: Mime): Future[MessageData] = withSelfUserFuture { selfUserId =>
    val tpe = mime match {
      case Mime.Video() => Message.Type.VIDEO_ASSET
      case Mime.Audio() => Message.Type.AUDIO_ASSET
      case _            => Message.Type.ANY_ASSET
    }
    addLocalMessage(MessageData(MessageId(assetId.str), convId, EventId.Zero, tpe, selfUserId))
  }

  def addRenameConversationMessage(convId: ConvId, selfUserId: UserId, name: String) =
    updateOrCreateLocalMessage(convId, Message.Type.RENAME, _.copy(name = Some(name)), MessageData(MessageId(), convId, EventId.Zero, Message.Type.RENAME, selfUserId, name = Some(name)))

  def addConnectRequestMessage(convId: ConvId, fromUser: UserId, toUser: UserId, message: String, name: String, fromSync: Boolean = false) = {
    val msg = MessageData(
      MessageId(), convId, EventId.Zero, Message.Type.CONNECT_REQUEST, fromUser, content = MessageData.textContent(message), name = Some(name), recipient = Some(toUser),
      time = if (fromSync) MessageData.UnknownInstant else Instant.now)

    if (fromSync) messagesStorage.insert(msg) else addLocalMessage(msg)
  }

  def addKnockMessage(convId: ConvId, selfUserId: UserId) = {
    debug(s"addKnockMessage($convId, $selfUserId)")
    addLocalMessage(MessageData(MessageId(), convId, EventId.Zero, Message.Type.KNOCK, selfUserId))
  }

  def addDeviceStartMessages(convs: Seq[ConversationData], selfUserId: UserId): Future[Set[MessageData]] =
    Serialized.future('addDeviceStartMessages)(traverse(convs filter isGroupOrOneToOne) { conv =>
      messagesStorage.getLastMessage(conv.id) map {
        case None =>    Some(MessageData(MessageId(), conv.id, EventId.Zero, Message.Type.STARTED_USING_DEVICE, selfUserId, time = Instant.EPOCH))
        case Some(_) => None
      }
    } flatMap { msgs =>
      messagesStorage.insert(msgs.flatten)
    })

  private def isGroupOrOneToOne(conv: ConversationData) = conv.convType == ConversationType.Group || conv.convType == ConversationType.OneToOne

  def addMessagesAfterVerificationUpdate(updates: Seq[(ConversationData, ConversationData)], convUsers: Map[ConvId, Seq[UserData]], changes: Map[UserId, VerificationChange]) =
    Future.traverse(updates) {
      case (prev, up) if up.verified == Verification.VERIFIED => addOtrVerifiedMessage(up.id)
      case (prev, up) if prev.verified == Verification.VERIFIED =>
        val convId = up.id
        val changedUsers = convUsers(convId).filter(!_.isVerified).flatMap { u => changes.get(u.id).map(u.id -> _) }
        val (users, change) =
          if (changedUsers.forall(c => c._2 == ClientAdded || c._2 == MemberAdded)) (changedUsers map (_._1), ClientAdded)
          else (changedUsers collect { case (user, ClientUnverified) => user }, ClientUnverified)

        val (self, other) = users.partition(_ == selfUserId)
        for {
          _ <- if (self.nonEmpty) addOtrUnverifiedMessage(convId, Seq(selfUserId), change) else Future.successful(())
          _ <- if (other.nonEmpty) addOtrUnverifiedMessage(convId, other, change) else Future.successful(())
        } yield ()
      case _ =>
        Future.successful(())
    }

  def addHistoryLostMessages(cs: Seq[ConversationData], selfUserId: UserId): Future[Set[MessageData]] = {
    // TODO: those messages should include information about what was actually changed
    traverseSequential(cs) { conv =>
      messagesStorage.getLastMessage(conv.id) map {
        case Some(msg) if msg.msgType != Message.Type.STARTED_USING_DEVICE =>
          Some(MessageData(MessageId(), conv.id, EventId.Zero, Message.Type.HISTORY_LOST, selfUserId, time = msg.time.plusMillis(1)))
        case _ =>
          // conversation has no messages or has STARTED_USING_DEVICE msg,
          // it means that conv was just created and we don't need to add history lost msg
          None
      }
    } flatMap { msgs =>
      messagesStorage.insert(msgs.flatten) flatMap { added =>
        // mark messages read if there is no other unread messages
        val times: Map[ConvId, Instant] = added.map(m => m.convId -> m.time) (breakOut)
        convs.storage.updateAll2(times.keys, { c =>
          val t = times(c.id)
          if (c.lastRead.toEpochMilli == t.toEpochMilli - 1) c.copy(lastRead = t) else c
        }) map { _ => added }
      }
    }
  }

  def addMemberJoinMessage(convId: ConvId, creator: UserId, users: Set[UserId]) = {
    verbose(s"addMemberJoinMessage($convId, $creator, $users)")

    def updateOrCreate(added: Set[UserId]) = {
      def update(msg: MessageData) = {
        msg.copy(members = msg.members ++ added)
      }
      def create = MessageData(MessageId(), convId, EventId.Zero, Message.Type.MEMBER_JOIN, creator, members = added)
      updateOrCreateLocalMessage(convId, Message.Type.MEMBER_JOIN, update, create)
    }

    // check if we have local leave message with same users
    messagesStorage.lastLocalMessage(convId, Message.Type.MEMBER_LEAVE) flatMap {
      case Some(msg) if users.exists(msg.members) =>
        val toRemove = msg.members -- users
        val toAdd = users -- msg.members
        if (toRemove.isEmpty) deleteMessage(msg) // FIXME: race condition
        else updateMessage(msg.id)(_.copy(members = toRemove)) // FIXME: race condition

        if (toAdd.isEmpty) successful(()) else updateOrCreate(toAdd)
      case _ =>
        updateOrCreate(users)
    }
  }

  def removeLocalMemberJoinMessage(convId: ConvId, users: Set[UserId]) = {
    messagesStorage.lastLocalMessage(convId, Message.Type.MEMBER_JOIN) flatMap {
      case Some(msg) =>
        val members = msg.members -- users
        if (members.isEmpty) {
          deleteMessage(msg)
        } else {
          updateMessage(msg.id) { _.copy(members = members) } // FIXME: possible race condition with addMemberJoinMessage or sync
        }
      case _ =>
        warn("removeLocalMemberJoinMessage: no local join message found")
        CancellableFuture.successful(())
    }
  }

  def addMemberLeaveMessage(convId: ConvId, selfUserId: UserId, user: UserId) = {
    // check if we have local join message with this user and just remove him from the list
    messagesStorage.lastLocalMessage(convId, Message.Type.MEMBER_JOIN) flatMap {
      case Some(msg) if msg.members == Set(user) => deleteMessage(msg) // FIXME: race condition
      case Some(msg) if msg.members(user) => updateMessage(msg.id)(_.copy(members = msg.members - user)) // FIXME: race condition
      case _ =>
        // check for local MemberLeave message before creating new one
        def newMessage = MessageData(MessageId(), convId, EventId.Zero, Message.Type.MEMBER_LEAVE, selfUserId, members = Set(user))
        def update(msg: MessageData) = msg.copy(members = msg.members + user)
        updateOrCreateLocalMessage(convId, Message.Type.MEMBER_LEAVE, update, newMessage)
    }
  }

  def addOtrVerifiedMessage(convId: ConvId): Future[Option[MessageData]] =
    messagesStorage.getLastMessage(convId) flatMap {
      case Some(msg) if msg.msgType == Message.Type.OTR_UNVERIFIED || msg.msgType == Message.Type.OTR_DEVICE_ADDED =>
        verbose(s"addOtrVerifiedMessage, removing previous message: $msg")
        messagesStorage.delete(msg.id) map { _ => None }
      case _ =>
        withSelfUserFuture { selfUser =>
          addLocalMessage(MessageData(MessageId(), convId, EventId.Zero, Message.Type.OTR_VERIFIED, selfUser), Status.SENT) map { Some(_) }
        }
    }

  def addOtrUnverifiedMessage(convId: ConvId, users: Seq[UserId], change: VerificationChange): Future[Option[MessageData]] = {
      val msgType = if (change == ClientUnverified) Message.Type.OTR_UNVERIFIED else Message.Type.OTR_DEVICE_ADDED
      withSelfUserFuture { selfUser =>
        addLocalSentMessage(MessageData(MessageId(), convId, EventId.Zero, msgType, selfUser, members = users.toSet)) map { Some(_) }
      }
    }

  def retryMessageSending(conv: ConvId, msgId: MessageId) =
    updateMessage(msgId) { msg =>
      if (msg.state == Status.SENT || msg.state == Status.PENDING) msg
      else msg.copy(state = Status.PENDING)
    } .flatMap {
      case Some(msg) => sync.postMessage(msg.id, conv) map (Some(_))
      case _ => successful(None)
    }

  def messageSent(convId: ConvId, msg: MessageData) =
    updateMessageState(convId, msg.id, Message.Status.SENT) andThen {
      case Success(Some(m)) => content.messagesStorage.onMessageSent ! m
    }

  def messageDeliveryFailed(convId: ConvId, msg: MessageData, error: ErrorResponse) =
    updateMessageState(convId, msg.id, Message.Status.FAILED) andThen {
      case Success(Some(m)) => content.messagesStorage.onMessageFailed ! (m, error)
    }

  def updateMessageState(convId: ConvId, messageId: MessageId, state: Message.Status) =
    updateMessage(messageId) { _.copy(state = state) }

  def markMessageRead(convId: ConvId, id: MessageId) =
    if (network.isOfflineMode) CancellableFuture.successful(None)
    else
      updateMessage(id) { msg =>
        if (msg.state == Status.FAILED) msg.copy(state = Status.FAILED_READ)
        else msg
      }
}
