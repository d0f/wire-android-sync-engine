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
package com.waz.mocked.notifications

import java.util.Date

import com.waz.api.MockedClientApiSpec
import com.waz.mocked.{MockBackend, SystemTimeline}
import com.waz.model.ConversationData.ConversationType
import com.waz.model._
import com.waz.testutils.Implicits._
import com.waz.threading.DispatchQueueStats
import org.scalatest.{BeforeAndAfterAll, FeatureSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class NotificationsSyncSpec extends FeatureSpec with Matchers with BeforeAndAfterAll with MockedClientApiSpec with MockBackend {
  import DefaultPushBehaviour.Implicit

  lazy val convs = api.getConversations
  val messagesCount = 5

  override val clientDelay: Timeout = Duration.Zero

  override protected def beforeAll(): Unit = {
    DispatchQueueStats.reset()
    val conn = addConnection()
    addMessageEvents(conn.convId, selfUserId, messagesCount)

    super.beforeAll()
  }

  scenario("initial sync") {
    withDelay(convs should not be empty)
    val msgs = zmessaging.messagesStorage.getEntries(ConvId(convs.head.getId))
    msgs.disableAutowiring()

    withDelay {
      convs should have size 1
      convs.head.getType shouldEqual ConversationType.OneToOne
      Await.result(zmessaging.syncRequests.content.syncStorage(_.getJobs), 1.second) shouldBe empty
      msgs.currentValue.map(_.size) shouldEqual Some(1)
    } (10.seconds)
  }

  feature("notifications sync") {

    val groupConvsCount = 10
    val one2OneConvsCount = 10
    val convsCount = one2OneConvsCount + groupConvsCount + 1
    lazy val prevConvs = convs.map(_.getId).toSet

    scenario("disconnect websocket") {
      api.onPause()
      withDelay(zmessaging.websocket.connected.currentValue shouldEqual Some(false)) // wait for websocket to disconnect
      prevConvs should have size 1 // save exisiting convs
    }

    scenario("generate notifications") {
      for (i <- 1 to one2OneConvsCount) {
        val conn = addConnection(time = SystemTimeline)
        addMessageEvents(conn.convId, selfUserId, messagesCount, s"1-1 test[$i] msg:", timeline = Some(SystemTimeline))
      }
      for (i <- 1 to groupConvsCount) {
        val members = getRandomConnections(5) ++: Seq.fill(5)(UserId())
        val conv = addGroupConversation(members, time = SystemTimeline)
        addMessageEvents(conv.remoteId, selfUserId, messagesCount, s"group test[$i] msg:", timeline = Some(SystemTimeline))
      }
      DispatchQueueStats.reset()
    }

    scenario("resume and wait for sync") {
      withDelay {
        val lastNotificationId = Await.result(zmessaging.push.lastNotification.lastNotificationId(), 1.second)
        lastNotificationId shouldBe 'defined
        lastNotificationId should not be notifications.lastOption.map(_.id)
      }
      val time = System.currentTimeMillis()
      api.onResume()

      withDelay {
        Await.result(zmessaging.push.lastNotification.lastNotificationId(), 1.second) shouldBe notifications.lastOption.map(_.id)
      }(10.seconds)

      info(f"Notifications processed in: ${(System.currentTimeMillis() - time) / 1000d} s")

      withDelay { convs should have size convsCount }

      val addedConvs = convs.filter(c => !prevConvs(c.getId))

      // check messages count in storage
      addedConvs foreach { c =>
        val msgs = zmessaging.messagesStorage.getEntries(c.id)
        msgs.disableAutowiring()
        try withDelay {
          withClue(s"${c.getType}, msgs: ${msgs.currentValue.get.map(m => (m.message.msgType, m.message.contentString))}") {
            msgs.currentValue.map(_.size) shouldEqual Some(msgCount(c.getType)) // FIXME: STARTED_USING_DEVICE and CONNECT_REQUEST should never be shown together in 1-1
          }
        } finally {
          println(s"conv: [${c.getType}] ${c.getId}")
          msgs.currentValue.get.zipWithIndex.foreach { case (m, i) => println(s"  $i: [${m.message.msgType}] ${m.message.contentString} at ${m.message.time}")}
        }
      }

      // check messages count in UI list
      val msgs = addedConvs.map(c => c -> c.getMessages)
      withDelay {
        msgs foreach { case (c, ms) =>
          ms should have size msgCount(c.getType)
        }
        Await.result(zmessaging.syncRequests.content.syncStorage(_.getJobs), 1.second) shouldBe empty
      } (10.seconds)

      info(s"Sync requests processed in: ${(System.currentTimeMillis() - time) / 1000} s")

      DispatchQueueStats.printStats()
    }
  }

  feature("Ignore failures during processing") {

    scenario("disconnect websocket 2") {
      api.onPause()
      withDelay(zmessaging.websocket.connected.currentValue shouldEqual Some(false))
    }

    scenario("generate invalid notifications") {
      addNotification(MemberUpdateEvent(Uid(), RConvId(), new Date, selfUserId, ConversationState()))
      addGroupConversation(Seq(UserId(), UserId()), time = SystemTimeline)
    }

    scenario("resume and create new conv, ignore invalid member update") {
      val count = convs.size()
      api.onResume()
      withDelay {
        convs should have size (count + 1)
      }
    }
  }

  def msgCount(t: ConversationType) = t match {
    case ConversationType.OneToOne => messagesCount + 3 // TODO: that's probably wrong with e2ee
    case ConversationType.Group => messagesCount + 1
    case ct => fail(s"unexpected conversation type: $ct")
  }
}
