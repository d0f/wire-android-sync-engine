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
package com.waz.service.conversation

import com.waz.ZLog._
import com.waz.api
import com.waz.api.MessageContent.Asset.ErrorHandler
import com.waz.api.MessageContent.Text
import com.waz.api.impl._
import com.waz.api.{ImageAssetFactory, Message, NetworkMode}
import com.waz.content._
import com.waz.model.ConversationData.ConversationType
import com.waz.model.GenericContent.Location
import com.waz.model.UserData.ConnectionStatus
import com.waz.model._
import com.waz.service._
import com.waz.service.assets.AssetService
import com.waz.service.call.VoiceChannelService
import com.waz.service.messages.MessagesService
import com.waz.service.tracking.TrackingEventsService
import com.waz.sync.SyncServiceHandle
import com.waz.threading.{CancellableFuture, Threading}
import com.waz.utils.Locales.currentLocaleOrdering
import com.waz.utils._

import scala.collection.breakOut
import scala.concurrent.Future
import scala.language.higherKinds

class ConversationsUiService(assets: AssetService, users: UserService, usersStorage: UsersStorage,
                             storage: ZmsDatabase, messages: MessagesService, members: MembersStorage, assetStorage: AssetsStorage,
                             convsContent: ConversationsContentUpdater, convStorage: ConversationStorage, network: NetworkModeService,
                             convs: ConversationsService, voice: VoiceChannelService, sync: SyncServiceHandle, lifecycle: ZmsLifecycle,
                             tracking: TrackingEventsService, errors: ErrorsService) {

  import ConversationsUiService._
  import Threading.Implicits.Background
  import assets._
  import convsContent._
  import messages._
  import users._

  def sendMessage(convId: ConvId, content: api.MessageContent): Future[Option[MessageData]] = {

    def mentionsMap(us: Array[api.User]): Future[Map[UserId, String]] =
      users.getUsers(us.map { u => UserId(u.getId) }) map { uss =>
        uss.map(u => u.id -> u.getDisplayName)(breakOut)
      }

    def sendTextMessage(m: String, mentions: Map[UserId, String] = Map.empty) =
      for {
        msg <- addTextMessage(convId, m, mentions)
        _   <- updateLastRead(msg)
        _   <- sync.postMessage(msg.id, convId)
      } yield Some(msg)

    def sendLocationMessage(loc: Location) =
      for {
        msg <- addLocationMessage(convId, loc)
        _   <- updateLastRead(msg)
        _   <- sync.postMessage(msg.id, convId)
      } yield Some(msg)

    def sendImageMessage(img: api.ImageAsset, conv: ConversationData) = {
      verbose(s"sendImageMessage($img, $conv)")
      if (img.getWidth > 0 && img.getHeight > 0) createMessageThenImage(AssetId(img.getId), img, conv)
      else createImageThenMessage(img, conv)
    }

    def createMessageThenImage(assetId: AssetId, img: api.ImageAsset, conv: ConversationData) =
      for {
        msg <- addImageMessage(convId, assetId, img.getWidth, img.getHeight)
        _   <- updateLastRead(msg)
        _   <- addImageAsset(assetId, img, conv.remoteId, isSelf = false)
        _   <- sync.postMessage(msg.id, convId)
      } yield Some(msg)

    def createImageThenMessage(img: api.ImageAsset, conv: ConversationData) =
      for {
        data <- addImageAsset(AssetId(), img, conv.remoteId, isSelf = false)
        msg  <- addImageMessage(convId, data.id, data.width, data.height)
        _    <- updateLastRead(msg)
        _    <- sync.postMessage(msg.id, convId)
      } yield Some(msg)

    def sendAssetMessage(in: AssetForUpload, conv: ConversationData, handler: ErrorHandler): Future[Option[MessageData]] = {

      def isFileTooLarge(size: Long, mime: Mime) = mime match {
        case Mime.Video() => false
        case _ => size > AssetData.MaxAllowedAssetSizeInBytes
      }

      def shouldWarnAboutFileSize(size: Long) =
        if (size < LargeAssetWarningThresholdInBytes) Future successful None
        else network.networkMode.head map {
          case api.NetworkMode.OFFLINE | api.NetworkMode.WIFI => None
          case net if lifecycle.isUiActive => Some(net)
          case _ => None
        }

      def showLargeFileWarning(size: Long, mime: Mime, net: NetworkMode, message: MessageData) = {
        Threading.assertUiThread()

        handler.noWifiAndFileIsLarge(size, net, new api.MessageContent.Asset.Answer {
          override def ok(): Unit = messages.retryMessageSending(convId, message.id)
          override def cancel(): Unit =
            messages.content.deleteMessage(message) flatMap { _ =>
              tracking.track(TrackingEvent.assetUploadCancelled(Some(size), mime.str))
            }
        })
      }

      def checkSize(size: Option[Long], mime: Mime, message: MessageData) = size match {
        case None => Future successful true
        case Some(s) if isFileTooLarge(s, mime) =>
          for {
            _ <- updateMessageState(convId, message.id, Message.Status.FAILED)
            _ <- errors.addAssetTooLargeError(convId, message.id)
            _ <- tracking.track(TrackingEvent.assetUploadFailed(ErrorResponse.internalError("asset too large")))
          } yield false
        case Some(s) =>
          shouldWarnAboutFileSize(s) flatMap {
            case Some(net) =>
              // will mark message as failed and ask user if it should really be sent
              // marking as failed ensures that user has a way to retry even if he doesn't respond to this warning
              // this is possible if app is paused or killed in meantime, we don't want to be left with message in state PENDING without a sync request
              updateMessageState(convId, message.id, Message.Status.FAILED) .map { _ =>
                showLargeFileWarning(s, mime, net, message)
                false
              } (Threading.Ui)
            case _ =>
              Future successful true
          }
      }

      for {
        mime        <- in.mimeType
        message     <- addAssetMessage(convId, in.id, mime)
        asset       <- addAsset(in, conv.remoteId)
        size        <- in.sizeInBytes
        isOtto      <- TrackingEventsService.isOtto(conv, usersStorage)
        _           <- tracking.track(TrackingEvent.assetUploadStarted(size, mime.str, conv.convType, isOtto))
        shouldSend  <- checkSize(size, mime, message)
        _ <- if (shouldSend) sync.postMessage(message.id, convId) else Future.successful(())
      } yield Some(message)
    }

    content match {
      case m: api.MessageContent.Text =>
        debug(s"send text message ${m.getContent}")
        if (m.getMentions.isEmpty) sendTextMessage(m.getContent)
        else mentionsMap(m.getMentions) flatMap { ms => sendTextMessage(m.getContent, ms) }
      case m: api.MessageContent.Location =>
        sendLocationMessage(Location(m.getLongitude, m.getLatitude, m.getName, m.getZoom))
      case m: api.MessageContent.Image =>
        convById(convId) flatMap {
          case Some(conv) => sendImageMessage(m.getContent, conv)
          case None       => Future.failed(new IllegalArgumentException(s"No conversation found for $convId"))
        }
      case m: api.MessageContent.Asset =>
        convById(convId) flatMap {
          case Some(conv) =>
            debug(s"send asset message ${m.getContent}")
            m.getContent match {
              case a @ ContentUriAssetForUpload(_, uri) =>
                a.mimeType .flatMap {
                  case m @ Mime.Image() =>
                    for {
                      size <- a.sizeInBytes
                      isOtto <- TrackingEventsService.isOtto(conv, usersStorage)
                    } tracking.track(TrackingEvent.imageUploadAsAsset(size, m.str, conv.convType, isOtto))
                    sendImageMessage(ImageAssetFactory.getImageAsset(uri), conv) // XXX: this has to run on UI thread
                  case _ =>
                    sendAssetMessage(a, conv, m.getErrorHandler)
                } (Threading.Ui)
              case a: AssetForUpload  => sendAssetMessage(a, conv, m.getErrorHandler)
            }
          case None       =>
            Future.failed(new IllegalArgumentException(s"No conversation found for $convId"))
        }
      case _ =>
        error(s"sendMessage($content) not supported yet")
        Future.failed(new IllegalArgumentException(s"MessageContent: $content is not supported yet"))
    }
  }

  def updateMessage(convId: ConvId, id: MessageId, content: Text): Future[Unit] = ???

  def deleteMessage(convId: ConvId, id: MessageId): Future[Unit] = for {
    _ <- messages.content.deleteOnUserRequest(Seq(id))
    _ <- sync.postDeleted(convId, id)
  } yield ()

  def recallMessage(convId: ConvId, id: MessageId): Future[Option[MessageData]] =
    messages.recallMessage(convId, id, users.selfUserId) flatMap {
      case Some(msg) =>
        sync.postRecalled(convId, msg.id, id) map { _ => Some(msg) }
      case None =>
        warn(s"could not recall message $convId, $id")
        Future successful None
    }

  private def updateLastRead(msg: MessageData) = updateConversationLastRead(msg.convId, msg.time)

  def setConversationArchived(id: ConvId, archived: Boolean): Future[Option[ConversationData]] = convs.setConversationArchived(id, archived)

  def setConversationMuted(id: ConvId, muted: Boolean): Future[Option[ConversationData]] =
    updateConversationMuted(id, muted) map {
      case Some((_, conv)) => sync.postConversationState(id, ConversationState(muted = Some(conv.muted), muteTime = Some(conv.muteTime))); Some(conv)
      case None => None
    }

  def setConversationName(id: ConvId, name: String): Future[Option[ConversationData]] = withSelfUserFuture { selfUserId =>
    verbose(s"setConversationName($id, $name)")
    updateConversationName(id, name) flatMap {
      case Some((_, conv)) if conv.name.contains(name) =>
        sync.postConversationName(id, conv.name.getOrElse(""))
        addRenameConversationMessage(id, selfUserId, name) map (_ => Some(conv))
      case conv =>
        warn(s"Conversation name could not be changed for: $id, conv: $conv")
        CancellableFuture.successful(None)
    }
  }

  def addConversationMembers(conv: ConvId, users: Seq[UserId]): Future[Seq[ConversationMemberData]] =
    withSelfUserFuture { selfUserId =>
      ifAbleToModifyMembers(conv, selfUserId, Seq.empty[ConversationMemberData]) {
        members.add(conv, users: _*) flatMap { members =>
          if (members.nonEmpty) {
            addMemberJoinMessage(conv, selfUserId, members.map(_.userId).toSet) map { _ =>
              sync.postConversationMemberJoin(conv, members.map(_.userId).toSeq)
              members.toSeq
            }
          } else Future.successful(members.toSeq)
        }
      }
    }

  def removeConversationMember(conv: ConvId, user: UserId): Future[Option[SyncId]] =
    withSelfUserFuture { selfUserId =>
      ifAbleToModifyMembers(conv, selfUserId, Option.empty[SyncId]) {
        members.remove(conv, user) flatMap { members =>
          if (members.isEmpty) Future.successful(None)
          else addMemberLeaveMessage(conv, selfUserId, members.head.userId) flatMap { _ =>
            sync.postConversationMemberLeave(conv, members.head.userId).map(Some(_))
          }
        }
      }
    }

  def leaveConversation(conv: ConvId): Future[Option[ConversationData]] = {
    verbose(s"leaveConversation($conv)")

    for {
      updated   <- setConversationStatusInactive(conv)
      _         <- withSelfUserFuture { removeConversationMember(conv, _) }
      archived  <- updateConversationArchived(conv, archived = true)
      _         <- voice.leaveVoiceChannel(conv)
    } yield archived.map(_._2).orElse(updated.map(_._2))
  }

  def ifAbleToModifyMembers[A, M[_]](conv: ConvId, user: UserId, zero: M[A])(f: => Future[M[A]]): Future[M[A]] = {
    val isGroup = convById(conv).map(_.exists(_.convType == ConversationType.Group))
    val isActiveMember = members.isActiveMember(conv, user)
    for {
      p1  <- isGroup
      p2  <- isActiveMember
      res <- if (p1 && p2) f else Future.successful(zero)
    } yield res
  }

  def clearConversation(id: ConvId): Future[Option[ConversationData]] = convById(id) flatMap {
    case Some(conv) if conv.convType == ConversationType.Group || conv.convType == ConversationType.OneToOne =>
      verbose(s"clearConversation($conv)")

      updateConversationCleared(conv.id, conv.lastEventTime) flatMap {
        case Some((_, c)) =>
          for {
            _ <- updateConversationLastRead(c.id, c.cleared)
            _ <- updateConversationArchived(c.id, archived = true)
            _ <- sync.postCleared(c.id, c.cleared)
          } yield Some(c)
        case None =>
          verbose("updateConversationCleared did nothing - already cleared")
          Future successful None
      }
    case Some(conv) =>
      warn(s"conversation of type ${conv.convType} can not be cleared")
      Future successful None
    case None =>
      warn(s"conversation to be cleared not found: $id")
      Future successful None
  }

  def getOrCreateOneToOneConversation(toUser: UserId): Future[ConversationData] =
    convById(ConvId(toUser.str)) flatMap {
      case Some(conv) => Future.successful(conv)
      case _ => withSelfUserFuture { selfUserId => createOneToOneConversation(toUser, selfUserId) }
    }

  /**
   * Creates conversation with already connected user.
   */
  def createOneToOneConversation(toUser: UserId, selfUserId: UserId) =
    usersStorage.get(toUser).flatMap {
      case Some(u) if u.connection == ConnectionStatus.Ignored =>
        createConversationWithMembers(ConvId(toUser.str), u.conversation.getOrElse(RConvId()), ConversationType.Incoming, toUser, Seq(selfUserId), hidden = true) flatMap { conv =>
          addMemberJoinMessage(conv.id, toUser, Set(selfUserId)) flatMap { _ =>
            u.connectionMessage.fold { Future.successful(conv) } { msg =>
              addConnectRequestMessage(conv.id, toUser, selfUserId, msg, u.name) map  { _ => conv }
            }
          }
        }
      case _ =>
        sync.postConversation(ConvId(toUser.str), Seq(toUser), None)
        createConversationWithMembers(ConvId(toUser.str), RConvId(), ConversationType.OneToOne, selfUserId, Seq(toUser)) flatMap { conv =>
          addMemberJoinMessage(conv.id, selfUserId, Set(toUser)) map (_ => conv)
        }
    }

  def createGroupConversation(id: ConvId, members: Seq[UserId]): Future[ConversationData] = withSelfUserFuture { selfUserId =>
    debug(s"createGroupConversation, id: $id, members: $members")
    createConversationWithMembers(id, ConversationsService.generateTempConversationId((selfUserId +: members).distinct: _*), ConversationType.Group, selfUserId, members) flatMap { conv =>
      debug(s"created: $conv")
      sync.postConversation(id, members, conv.name)
      addMemberJoinMessage(conv.id, selfUserId, members.toSet) map (_ => conv)
    }
  }

  def findGroupConversations(prefix: SearchKey, limit: Int): Future[Seq[ConversationData]] =
    withSelfUserFuture(id => convStorage.search(prefix, id)).map(_.sortBy(_.displayName)(currentLocaleOrdering).take(limit))

  def knock(id: ConvId): Future[Option[MessageData]] = withSelfUserFuture { selfUserId =>
    Serialized.future("knock", id) {
      getActiveKnockMessage(id, selfUserId) flatMap {
        case Some(msg) if msg.hotKnock => CancellableFuture.successful(None) // ignore - hot knock not expired
        case Some(msg) => // change to hot knock
          sync.postMessage(msg.id, id)
          updateKnockToHotKnock(msg.id)
        case _ =>
          addKnockMessage(id, selfUserId) map { msg =>
            sync.postMessage(msg.id, id)
            Some(msg)
          }
      }
    }
  }

  def setLastRead(convId: ConvId, msg: MessageData): Future[Option[ConversationData]] = {
    updateConversationLastRead(convId, msg.time) map {
      case Some((_, conv)) =>
        sync.postLastRead(convId, conv.lastRead)
        Some(conv)
      case _ => None
    }
  }
}

object ConversationsUiService {
  private implicit val logTag: LogTag = logTagFor[ConversationsUiService]

  val LargeAssetWarningThresholdInBytes = 3145728L // 3MiB
}
