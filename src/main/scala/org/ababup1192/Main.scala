package org.ababup1192

object Main {
  var currentUser: Option[User] = None

  def main(args: Array[String]): Unit = {
    println("\u001b[H\u001b[2J")
    printMessage()
    // 入力をEOFまで待ち続ける。
    Iterator.continually(scala.io.StdIn.readLine()).takeWhile(l => l != null).foreach {
      line =>
        commandSelect(line)
    }
    exit()
  }

  def commandSelect(command: String): Unit = {
    val createUserRule = """c:\s*(\w+),\s*(\w+|[^\x01-\x7E]+),\s*(\w+@[\w\.]+),\s*([0-9]+-[0-9]+-[0-9]+)""".r
    val changeUserRule = """ch:\s*(\w+)""".r
    val followUserByEmailRule = """f:\s*(\w+@[\w\.]+)""".r
    val followUserByTelRule = """f:\s*([0-9]+-[0-9]+-[0-9]+)""".r
    val sendMessageRule = """s:\s*(\w+),\s*(\w+|[^\x01-\x7E]+)""".r

    (command.trim match {
      case "?" | "help" => HelpCommand
      case "" => EmptyCommand
      case createUserRule(id, name, email, tel) => CreateUserCommand(id, name, email, tel)
      case changeUserRule(id) => ChangeUserCommand(id.trim)
      case "l" => ListFriends(currentUser)
      case followUserByEmailRule(email) => FollowCommand(currentUser, email)
      case followUserByTelRule(tel) => FollowCommand(currentUser, tel)
      case "t" => ShowTalkCommand(currentUser)
      case sendMessageRule(userId, message) => SendMessageCommand(currentUser, userId, message)
      case "w" => WriteLogCommand
      case _ => CommandError
    }).run()
  }


  def printMessage(): Unit = {
    printMessage("")
  }

  def printMessage(message: String): Unit = {
    println("\u001b[H\u001b[2J")
    println("*****Scalin*****")
    val userStr = currentUser.map(user => s"ユーザ [${user.id}, ${user.name}]").getOrElse("[No User]")
    print(userStr + "\n\n\n\n")
    print(message)
    val line: Int = message.lines.length
    for (_ <- 0 to (12 - line)) {
      println("\n")
    }
    print("コマンド?  ")
  }

  def exit(): Unit = {
    println("\u001b[h\u001b[2j")
    println("[scalineは、終了しました。]")
  }

}
