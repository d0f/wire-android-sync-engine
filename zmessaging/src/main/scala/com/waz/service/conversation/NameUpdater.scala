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

import android.content.Context
import com.waz.ZLog._
import com.waz.content._
import com.waz.model.ConversationData.ConversationType
import com.waz.model.UserData.ConnectionStatus
import com.waz.model.{ConvId, UserData, UserId}
import com.waz.service.UserService
import com.waz.threading.SerialDispatchQueue
import com.waz.utils.events.EventContext
import com.waz.utils.{BiRelation, ThrottledProcessingQueue}
import com.waz.zms.R

import scala.collection.{GenTraversable, breakOut}
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try

/**
 * Updates conversation names when any dependency changes (members list, user names).
 */
class NameUpdater(context: Context, users: UserService, usersStorage: UsersStorage, convs: ConversationStorage, membersStorage: MembersStorage) {

  private implicit val tag: LogTag = logTagFor[NameUpdater]
  private implicit val ev = EventContext.Global
  private implicit val dispatcher = new SerialDispatchQueue(name = "NameUpdaterQueue")

  import users._

  private lazy val emptyConversationName = Try(context.getResources.getString(R.string.zms_empty_conversation_name)).getOrElse("Empty conversation")

  // unnamed group conversations with active members
  // we are keeping that in memory, it should be fine,
  // ppl usually don't have many unnamed group conversations (especially with many users)
  private var groupConvs = Set.empty[ConvId]
  private var groupMembers = BiRelation.empty[ConvId, UserId]

  private val queue = new ThrottledProcessingQueue[Any](500.millis, { ids =>
    withSelfUserFuture { selfUser => updateGroupNames(selfUser, ids.toSet) }
  }, "GroupConvNameUpdater")

  // load groups and members
  lazy val init = for {
    all <- convs.getAll
    groups = all.filter(c => c.convType == ConversationType.Group && c.name.isEmpty)
    members <- Future.traverse(groups) { c => membersStorage.getActiveUsers(c.id) map (c.id -> _) }
  } yield {
    groupConvs ++= groups.map(_.id)
    addMembers(members)
  }

  def registerForUpdates(): Unit = {

    usersStorage.onAdded { onUsersChanged(_) }
    usersStorage.onUpdated { updates =>
      onUsersChanged(updates.collect {
        case (prev, current) if prev.name != current.name || prev.displayName != current.displayName => current
      })
    }

    convs.onAdded { cs =>
      val unnamedGroups = cs.collect { case c if c.convType == ConversationType.Group && c.name.isEmpty => c.id }
      if (unnamedGroups.nonEmpty) {
        init map { _ =>
          groupConvs ++= unnamedGroups
          addMembersForConvs(unnamedGroups)
        }
      }
    }

    convs.onUpdated { updates =>
      val changedGroups = updates.collect {
        case (prev, conv) if conv.convType == ConversationType.Group && prev.name.isDefined != conv.name.isDefined => conv
      }

      if (changedGroups.nonEmpty) {
        val (named, unnamed) = changedGroups.partition(_.name.isDefined)
        val namedIds = named.map(_.id)
        val unnamedIds = unnamed.map(_.id)

        init map { _ =>
          groupConvs = groupConvs -- namedIds ++ unnamedIds
          if (named.nonEmpty)
            groupMembers = groupMembers.removeAllLeft(namedIds)
          if (unnamed.nonEmpty)
            addMembersForConvs(unnamedIds) map { _ => queue.enqueue(unnamedIds)}
        }
      }
    }

    membersStorage.onChanged { members =>
      init map { _ =>
        val ms = members.filter(m => groupConvs(m.convId))
        val (active, inactive) = ms.partition(_.active)
        groupMembers = groupMembers -- inactive.map(m => m.convId -> m.userId) ++ active.map(m => m.convId -> m.userId)

        queue.enqueue(ms.map(_.convId).distinct)
      }
    }
  }

  def forceNameUpdate(id: ConvId) = convs.get(id) flatMap {
    case Some(conv) =>
      users.withSelfUserFuture { selfUserId =>
        for {
          members <- membersStorage.get(conv.id)
          users <- usersStorage.getAll(members.map(_.userId).filter(_ != selfUserId))
          name = generatedName(users.map(_.map(_.getDisplayName)))
          _ <- convs.update(conv.id,  _.copy(generatedName = name))
        } yield ()
      }
    case None =>
      Future successful None
  }

  private def addMembersForConvs(convs: Traversable[ConvId]) =
    Future.traverse(convs) { c => membersStorage.getActiveUsers(c) map (c -> _) } map addMembers

  private def addMembers(members: Traversable[(ConvId, Seq[UserId])]) =
    groupMembers ++= members.flatMap { case (c, us) => us.map(c -> _) }


  private def onUsersChanged(users: Seq[UserData]) = {

    def updateGroups() = queue.enqueue(users.map(_.id))

    def updateOneToOnes() = {
      val names: Map[ConvId, String] = users.collect {
        case u if u.connection != ConnectionStatus.Unconnected => ConvId(u.id.str) -> u.name // one to one use full name
      } (breakOut)

      if (names.isEmpty) Future successful Nil
      else convs.updateAll2(names.keys, { c => c.copy(generatedName = names(c.id)) })
    }

    updateGroups()
    updateOneToOnes()
  }


  private def updateGroupNames(selfUser: UserId, ids: Set[Any]) = init flatMap { _ =>
    val convIds = ids flatMap {
      case id: ConvId => Seq(id)
      case id: UserId => groupMembers.foreset(id)
      case _ => Nil
    }

    val members: Map[ConvId, Seq[UserId]] = convIds.map { id => id -> groupMembers.afterset(id).toSeq } (breakOut)
    val users = members.flatMap(_._2)

    usersStorage.getAll(users) flatMap { uds =>
      val names: Map[UserId, Option[String]] = users.zip(uds.map(_.map(_.getDisplayName)))(breakOut)
      val convNames = members.mapValues { users => generatedName(users.filterNot(_ == selfUser) map names) }
      convs.updateAll2(convIds, { c => c.copy(generatedName = convNames(c.id)) })
    }
  }

  private def generatedName(userNames: GenTraversable[Option[String]]): String = {
    val generated = userNames.flatten.filter(_.nonEmpty).mkString(", ")
    if (generated.isEmpty) emptyConversationName else generated
  }
}

object NameUpdater {
  def generatedName(convType: ConversationType)(users: GenTraversable[UserData]): String =
    users.map(user => if (convType == ConversationType.Group) user.getDisplayName else user.name).filter(_.nonEmpty).mkString(", ")
}
