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
package com.waz.conv

import java.lang.Iterable

import akka.pattern.ask
import android.database.sqlite.SQLiteDatabase
import com.waz.ZLog._
import com.waz.api.ConversationsList.ConversationCallback
import com.waz.api.ErrorsList.{ErrorDescription, ErrorListener}
import com.waz.api.IConversation.Type._
import com.waz.api.impl.ImageAsset
import com.waz.api._
import com.waz.model.ErrorData.ErrorDataDao
import com.waz.model._
import com.waz.provision.ActorMessage._
import com.waz.testutils.DefaultPatienceConfig
import com.waz.testutils.Matchers._
import com.waz.testutils.Implicits._
import com.waz.utils._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FeatureSpec, Matchers, OptionValues}

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._

class ConversationsSpec extends FeatureSpec with Matchers with OptionValues with ProvisionedApiSpec with ProcessActorSpec with ScalaFutures with DefaultPatienceConfig { test =>

  private implicit val logTag: LogTag = logTagFor[ConversationsSpec]

  override val provisionFile = "/conversations.json"

  lazy val conversations = api.getConversations
  lazy val self = api.getSelf
  lazy val groupConv = getConv(GROUP)
  lazy val selfConv = api.getConversations.getSelfConversation
  lazy val auto2User = api.getUser(provisionedUserId("auto2").str)
  lazy val auto2Conv = conversations.getConversation(provisionedUserId("auto2").str)
  lazy val conv = groupConv

  def getConv(p: IConversation => Boolean, msg: String = ""): IConversation = {
    withDelay(conversations.size should be > 0)

    val conv = conversations.find(p)
    withClue(msg + conversations.map(c => c.getName + " " + c.getType).mkString(" [", ",", "]")) {
      conv should be('defined)
    }
    conv.get
  }

  lazy val auto2 = registerDevice("ConversationsSpec_auto2")
  lazy val otherClient = registerDevice("ConversationsSpec_otherClient")

  implicit def db: SQLiteDatabase = zmessaging.db.dbHelper.getWritableDatabase

  def getConv(convType: IConversation.Type): IConversation = getConv(_.getType == convType, s"looking for conv type: $convType")

  scenario("initial sync") {
    withDelay(conversations.size shouldEqual 5)
  }

  scenario("Init other client") {
    auto2 ? Login(provisionedEmail("auto2"), "auto2_pass") should eventually(be(Successful))
    otherClient ? Login(email, password) should eventually(be(Successful))
  }

  feature("Create group conversation") {

    scenario("Create conversation") {
      withDelay(groupConv.getUsers should not be empty)

      val users = List("auto2", "auto3").map(k => api.getUser(provisionedUserId(k).str))
      val count = conversations.size()
      var conv = Option.empty[IConversation]
      conversations.createGroupConversation(users.asJava, new ConversationCallback {
        override def onConversationsFound(conversations: Iterable[IConversation]): Unit = conv = conversations.asScala.headOption
      })

      withDelay {
        withClue(conversations.map(_.data).mkString("[", ",", "]")) {
          conversations should have size(count + 1)
        }
      }
      conversations should have size(count + 1)

      withDelay {
        conv.value.data.lastEvent should be > EventId.Zero
      }
      awaitUi(5.seconds)
    }

    scenario("Receive conversation created by other user") {
      val count = conversations.size()
      (auto2 ? CreateGroupConversation(UserId(self.getUser.getId), provisionedUserId("auto3"))) .futureValue shouldEqual Successful

      withDelay { conversations should have size (count + 1) }

      val c = conversations.get(0) // new conversation should be on top
      val ms = c.getUsers

      withDelay {
        ms should have size 2
        ms.map(_.getName).toSet shouldEqual Set("auto2 user", "auto3 user")
      }
    }

    scenario("Receive conversation created on other client") {
      val count = conversations.size()
      (otherClient ? CreateGroupConversation(provisionedUserId("auto2"), provisionedUserId("auto3"))) .futureValue shouldEqual Successful

      withDelay(conversations should have size (count + 1))

      val c = conversations.get(0) // new conversation should be on top
      val ms = c.getUsers

      withDelay {
        ms should have size 2
        ms.map(_.getName).toSet shouldEqual Set("auto2 user", "auto3 user")
      }
    }

    scenario("Try creating conversation with non-existing user") {
      val users = List(provisionedUserId("auto2"), provisionedUserId("auto3"), UserId()).map(id => api.getUser(id.str))

      var receivedError = None: Option[ErrorDescription]
      val errors = api.getErrors
      errors.addErrorListener(new ErrorListener {
        override def onError(error: ErrorDescription): Unit = receivedError = Some(error)
      })
      val count = conversations.size()
      var msgs = Option.empty[MessagesList]
      conversations.createGroupConversation(users.asJava, new ConversationCallback {
        override def onConversationsFound(conversations: Iterable[IConversation]): Unit = msgs = conversations.asScala.headOption map (_.getMessages)
      })

      withDelay {
        errors should have size 1
        receivedError should be('defined)
        withClue(conversations.map(c => (c.getName, c.getType)).mkString(", ")) {
          conversations should have size (count + 1)
        }
        msgs.value should not be empty
      }
      // dismiss error
      receivedError.get.dismiss()
      withDelay {
        errors should be(empty)
        conversations should have size count
        msgs.value should be(empty)
      }
      ErrorDataDao.list should be(empty)
    }
  }

  feature("Conversation background on first sync") {

    scenario("set user picture") {
      self.setPicture(api.ui.images.createImageAssetFrom(IoUtils.toByteArray(getClass.getResourceAsStream("/images/penguin.png"))))

      withPush {
        case UserUpdateEvent(_, info) => info.picture.isDefined
      } {
        auto2 ? UpdateProfileImage("/images/penguin.png") should eventually(be(Successful))
      } (45.seconds)

      withDelay(auto2User.getPicture should not be empty)
    }

    scenario("Get background for self conversation") {
      withDelay(selfConv.getBackground should be (empty))
    }

    scenario("Get background for one-to-one conversation") {
      val members = auto2Conv.getUsers
      withDelay {
        members should have size 1
        members should contain(auto2User)
        auto2User.getPicture should not be empty
        auto2Conv.getBackground shouldEqual ImageAsset.Empty
      }
    }

    scenario("Get background for group conversation") {
      debug(s"gotConv: ${conv.getId}")

      info(s"gotConv: ${conv.getName}, id: ${conv.getId}")

      val messages = conv.getMessages
      withDelay(messages should not be empty)

      var lastMsg = messages.last
      var lastUser = lastMsg.getUser

      def lastMessagePicture = {
        val l = messages.last
        if (l != lastMsg) lastMsg = l
        lastUser = lastMsg.getUser
        lastUser.getPicture
      }

      withClue(s"lastUser name: ${lastUser.getName}, id: ${lastUser.getId}, accent: ${lastUser.getAccent}, pic: ${lastUser.getPicture}, lastMsg: ${lastMsg.getBody}") {
        withDelay(lastMessagePicture should not be ImageAsset.Empty)(15.seconds)
      }

      info(s"lastMessage: ${lastMsg.getBody}, lastUser: ${lastUser.getName}")

      withDelay(conv.getBackground should be (ImageAsset.Empty))(10.seconds)
    }
  }

  feature("Conversation name") {

    scenario("Change group conversation name") {
      val name = if (conv.getName == "test conv") "test conv 1" else "test conv"
      conv.setConversationName(name)
      conv.name = "other"

      withDelay(conv.getName shouldEqual name)
    }
  }

  feature("Conversation members") {

    scenario("Remove member from conversation") {
      val members = conv.getUsers

      withDelay(members should have size 2)
      withDelay(members.map(_.getName) should contain("auto2 user"))

      val user = members.find(_.getName == "auto2 user").get

      conv.removeMember(user)
      withDelay(members should have size 1)
    }

    scenario("Add member to conversation") {
      val members = conv.getUsers
      val msgs = conv.getMessages
      withDelay(members should have size 1)

      val user = api.getUser(provisionedUserId("auto2").str)

      conv.addMembers(List(user.asInstanceOf[com.waz.api.User]).asJava)
      withDelay(members should have size 2)

      withDelay {
        msgs.getLastMessage.getMessageType shouldEqual Message.Type.MEMBER_JOIN
        msgs.getLastMessage.data.source.nonLocal shouldEqual true
      }
    }

    scenario("Try adding not connected user to conversation") {
      val members = conv.getUsers
      val msgs = conv.getMessages
      val errors = api.getErrors
      errors.dismissAll()

      withDelay {
        members should have size 2
        msgs should not be empty
        errors should be(empty)
      }

      awaitUi(1.second)
      val count = msgs.size()

      conv.addMembers(List(api.getUser(provisionedUserId("auto5").str).asInstanceOf[com.waz.api.User]).asJava)

      withDelay {
        errors should have size 1
        members should have size 2
        msgs should have size count
      }
      val error = errors.head
      info(s"got error: ${error.getResponse}")
      error.getType shouldEqual ErrorType.CANNOT_ADD_UNCONNECTED_USER_TO_CONVERSATION
      error.dismiss()

      withDelay {
        msgs should have size count
        members should have size 2
        errors should be(empty)
      }
    }

    scenario("Receive new conversation when other user adds me to group conversation") {
      val count = conversations.size()
      val convId = Await.result(auto2 ? GetConv("conversation 6"), timeout).asInstanceOf[Successful].response

      auto2 ? AddMembers(RConvId(convId), UserId(self.getUser.getId)) should eventually(be(Successful))

      withDelay {
        conversations should have size (count + 1)
        conversations.map(_.data.remoteId) should contain(RConvId(convId))
      }
    }
  }

  feature("Conversation state") {

    scenario("Archive conversation") {
      auto2Conv.isArchived shouldEqual false
      auto2Conv.setArchived(true)
      withDelay {
        conversations should not contain auto2Conv
        auto2Conv.isArchived shouldEqual true
      }
      awaitUi(1.second)
      auto2Conv.isArchived shouldEqual true
    }

    scenario("Unarchive conversation") {
      auto2Conv.isArchived shouldEqual true

      var wasArchived = false
      val listener = new UpdateListener {
        override def updated(): Unit = wasArchived ||= auto2Conv.isArchived
      }
      auto2Conv.addUpdateListener(listener)
      auto2Conv.setArchived(false)
      withDelay {
        conversations should contain(auto2Conv)
        auto2Conv.isArchived shouldEqual false
      }
      awaitUi(1.second)
      wasArchived shouldEqual false
    }

    scenario("Archive conversation on other client") {
      auto2Conv.isArchived shouldEqual false
      otherClient ? ArchiveConv(auto2Conv.data.remoteId) should eventually(be(Successful))
      withDelay {
        conversations should not contain auto2Conv
        auto2Conv.isArchived shouldEqual true
      }
    }

    scenario("Unarchive conversation from other client") {
      auto2Conv.isArchived shouldEqual true
      otherClient ? UnarchiveConv(auto2Conv.data.remoteId) should eventually(be(Successful))
      withDelay {
        conversations should contain(auto2Conv)
        auto2Conv.isArchived shouldEqual false
      }
    }

    scenario("Mute conversation") {
      auto2Conv.isMuted shouldEqual false
      otherClient ? MuteConv(auto2Conv.data.remoteId) should eventually(be(Successful))
      withDelay(auto2Conv.isMuted shouldEqual true)
    }

    scenario("Unmute conversation") {
      auto2Conv.isMuted shouldEqual true
      otherClient ? UnmuteConv(auto2Conv.data.remoteId) should eventually(be(Successful))
      withDelay(auto2Conv.isMuted shouldEqual false)
    }
  }
}
