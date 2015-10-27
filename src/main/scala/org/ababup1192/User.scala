package org.ababup1192

case class User(id: String, name: String, email: String, tel: String) {
}

case class UserRepository(users: List[User])

object UserRepository {
  private[this] var repo: UserRepository = new UserRepository(List[User]())

  def addUser(user: User): Unit = {
    repo = repo.copy(user :: repo.users)
  }

  // IDからユーザから取得
  def findByUserId(id: String): Option[User] = {
    repo.users.find(_.id == id)
  }

  def findByUserIdList(idList: List[String]): List[User] = {
    repo.users.filter(user => idList.contains(user.id))
  }

  def findByEmailOrTel(keyword: String): Option[User] = {
    repo.users.find(user => user.email == keyword || user.tel == keyword)
  }

  def exists(id: String): Boolean = {
    repo.users.exists(_.id == id)
  }

}