package jp.t2v.lab.play2.auth

import scala.annotation.tailrec
import scala.util.Random
import java.security.SecureRandom
import javax.inject.Inject

import play.api.cache.SyncCacheApi

import scala.reflect.ClassTag
import scala.concurrent.duration._
class CacheIdContainer[Id: ClassTag] @Inject()(implicit val syncCacheApi: SyncCacheApi) extends IdContainer[Id] {

  private[auth] val tokenSuffix = ":token"
  private[auth] val userIdSuffix = ":userId"
  private[auth] val random = new Random(new SecureRandom())

  def startNewSession(userId: Id, timeoutInSeconds: Int): AuthenticityToken = {
    removeByUserId(userId)
    val token = generate
    println(s"Generated new token\n$token")
    store(token, userId, timeoutInSeconds)
    token
  }

  @tailrec
  private[auth] final def generate: AuthenticityToken = {
    val table = "abcdefghijklmnopqrstuvwxyz1234567890_.~*'()"
    val token = Iterator.continually(random.nextInt(table.size)).map(table).take(64).mkString
    if (get(token).isDefined) generate else token
  }

  private[auth] def removeByUserId(userId: Id) {
    syncCacheApi.get[String](userId.toString + userIdSuffix) foreach unsetToken
    unsetUserId(userId)
  }

  def remove(token: AuthenticityToken) {
    get(token) foreach unsetUserId
    unsetToken(token)
  }

  private[auth] def unsetToken(token: AuthenticityToken) {
    println(s"Removing token\n $token")
    syncCacheApi.remove(token + tokenSuffix)
  }
  private[auth] def unsetUserId(userId: Id) {
    syncCacheApi.remove(userId.toString + userIdSuffix)
  }

  def get(token: AuthenticityToken) = {
    val id = syncCacheApi.get(token + tokenSuffix).map(_.asInstanceOf[Id])
    println(s"Getting from cache the token\n $token\tvalue\t$id")
    id
  }

  private[auth] def store(token: AuthenticityToken, userId: Id, timeoutInSeconds: Int) {
    println(s"Storing token\n $token")
    syncCacheApi.set(token + tokenSuffix, userId, Duration(timeoutInSeconds,SECONDS))
    syncCacheApi.set(userId.toString + userIdSuffix, token, Duration(timeoutInSeconds,SECONDS))
  }

  def prolongTimeout(token: AuthenticityToken, timeoutInSeconds: Int) {
    get(token).foreach(store(token, _, timeoutInSeconds))
  }

}
