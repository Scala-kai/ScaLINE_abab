package org.ababup1192

import java.util.Date

sealed trait Event

case class CreateUserEvent(user: User, date: Date) extends Event

case class FollowEvent(follow: Follow, date: Date) extends Event

case class SendMessageEvent(msg: Message, date: Date) extends Event

case class EventRepository(events: List[Event])

object EventRepository {
  private[this] var repo: EventRepository = new EventRepository(List[Event]())

  def addEvent(event: Event): Unit = {
    repo = repo.copy(event :: repo.events)
  }

  def printLog(): String = {
    repo.events.reverse.mkString("\n")
  }
}
