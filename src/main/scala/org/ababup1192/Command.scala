package org.ababup1192

import java.util.Date

sealed trait Command {
  def run(): Unit
}

case object HelpCommand extends Command {
  override def run(): Unit = {
    Main.printMessage(
      """
          Create User        ->  c: [id], [name], [email], [tel]
          Change User        ->  ch: [id]
          List Friends       ->  l
          Follow User        ->  f: [Email or Tel(No Hyphen)]
          Show Talk          ->  t
          Send Message       ->  s: [Friend User ID], [Message]
          Write Log          ->  w
          Exit               ->  C-D
          Help               ->  ? or help
      """)
  }
}

case object EmptyCommand extends Command {
  override def run(): Unit = {
    Main.printMessage()
  }
}

case object CommandError extends Command {
  override def run(): Unit = {
    Main.printMessage("[Error]: そのコマンドは存在しません。")
  }
}

case class CreateUserCommand(id: String, name: String, email: String, tel: String) extends Command {
  override def run(): Unit = {
    if (UserRepository.exists(id)) {
      Main.printMessage("[Error]: そのユーザは既に存在します。")
    } else {
      val user = User(id, name, email, tel)
      EventRepository.addEvent(CreateUserEvent(user, new Date()))
      UserRepository.addUser(User(id, name, email, tel))
      Main.printMessage(s"${user}を作成しました。")
    }
  }
}

case class ChangeUserCommand(id: String) extends Command {
  override def run(): Unit = {
    UserRepository.findByUserId(id) match {
      case Some(user) =>
        Main.currentUser = Some(user)
        Main.printMessage("ユーザを変更しました。")
      case None =>
        Main.printMessage("[Error]: そのユーザは存在しません。")
    }
  }
}

case class ListFriends(user: Option[User]) extends Command {
  override def run(): Unit = {
    Main.printMessage(
      user match {
        case Some(u) =>
          // List[ID] -> List[User]
          UserRepository.findByUserIdList(
            // ID -> List[ID]
            FollowRepository.findUsersIdList(u.id)
          ).mkString("\n")
        case None =>
          "[Error: ユーザが選択されていません。"
      }
    )
  }
}

case class FollowCommand(from: Option[User], keyword: String) extends Command {
  override def run(): Unit = {
    UserRepository.findByEmailOrTel(keyword) match {
      case Some(to) =>
        from match {
          case Some(f) =>
            Main.printMessage(s"${f}をフォローしました。")
            FollowEvent(Follow(f.id, to.id), new Date())
            FollowRepository.addFollow(Follow(f.id, to.id))
            FollowRepository.addFollow(Follow(to.id, f.id))
          case None =>
            Main.printMessage("[Error]: ユーザが選択されていません。")
        }
      case None =>
        Main.printMessage("[Error]: そのユーザは存在しません。")
    }
  }
}

case class ShowTalkCommand(currentUser: Option[User]) extends Command {
  override def run(): Unit = {
    currentUser match {
      case Some(user) =>
        Main.printMessage(MessageRepository.showMessage(user))
      case None =>
        Main.printMessage("[Error]: そのユーザは存在しません。")
    }
  }
}

case class SendMessageCommand(from: Option[User], toUserId: String, body: String) extends Command {
  override def run(): Unit = {
    UserRepository.findByUserId(toUserId) match {
      case Some(to) =>
        from match {
          case Some(f) =>
            if (FollowRepository.isFollow(f.id, to.id)) {
              val message = UnReadMessage(f, to, body, new Date())
              Main.printMessage(s"${to.name} -> $body [${new Date()}]")
              EventRepository.addEvent(SendMessageEvent(message, new Date()))
              MessageRepository.addMessage(message)
            } else {
              Main.printMessage("[Error]: そのユーザはフォローしていません。")
            }
          case None =>
            Main.printMessage("[Error]: ユーザが選択されていません。")
        }
      case None =>
        Main.printMessage("[Error]: そのユーザは存在しません。")
    }
  }
}

case object WriteLogCommand extends Command {
  override def run(): Unit = {
    Main.printMessage(EventRepository.printLog())
  }
}



