package org.ababup1192

case class Follow(from: String, to: String)

case class FollowRepository(follows: List[Follow])

object FollowRepository {
  private[this] var repo: FollowRepository = new FollowRepository(List[Follow]())

  def addFollow(follow: Follow): Unit = {
    repo = repo.copy(follow :: repo.follows)
  }

  def findUsersIdList(id: String): List[String] = {
    repo.follows.filter(_.from == id).map(_.to)
  }

  def isFollow(fromId: String, toId: String): Boolean = {
    repo.follows.exists(follow => follow.from == fromId && follow.to == toId)
  }

}