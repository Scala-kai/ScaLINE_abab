package org.ababup1192

import java.util.Date

trait Message {
  val from: User
  val to: User
  val body: String
}

case class UnReadMessage(from: User, to: User, body: String, date: Date) extends Message {

  def getMessage(user: User): String = {
    if (user == from) {
      s"${from.name}: $body [$date]"
    } else {
      s"${from.name}: $body [$date]"
    }
  }
}

case class ReadMessage(from: User, to: User, body: String, date: Date) extends Message {
  def getMessage(user: User): String = {
    if (user == from) {
      s"既読 ${from.name}: $body [$date]"
    } else {
      s"${from.name}: $body [$date]"
    }
  }
}

case class MessageRepository(messages: List[Message])

object MessageRepository {
  private[this] var repo: MessageRepository = new MessageRepository(List[Message]())

  def addMessage(message: Message): Unit = {
    repo = repo.copy(message :: repo.messages)
  }

  // あるユーザが確認するべき、全てのメッセージを持ってくる。
  def findByUser(user: User): List[Message] = {
    repo.messages.filter(msg => msg.from == user || msg.to == user)
  }

  def readMessage(user: User, index: Int, msg: UnReadMessage): Unit = {
    if (msg.to == user) {
      repo = repo.copy(repo.messages.updated(index, ReadMessage(msg.from, msg.to, msg.body, msg.date)))
    }
  }

  // メッセージの表示
  def showMessage(user: User): String = {
    findByUser(user).zipWithIndex.map {
      case (msg@UnReadMessage(from, to, body, date), index) =>
        // 送信者が自分の場合
        if (user == from) {
          msg.getMessage(from)
          // 相手のメッセージ
        } else {
          // 相手のメッセージが未読の場合、既読にする。
          readMessage(user, index, msg)
          msg.getMessage(to)
        }
      case (msg@ReadMessage(from, to, body, date), index) =>
        if (user == from) {
          msg.getMessage(from)
        } else {
          msg.getMessage(to)
        }
    }.reverse.mkString("\n")
  }


}