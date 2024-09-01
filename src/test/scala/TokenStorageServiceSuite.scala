import zio.*
import zio.test.*
import zio.test.Assertion._

import java.util.concurrent.TimeUnit

object TokenStorageServiceSuite extends ZIOSpecDefault:
  def spec = suite("getOrCreateToken") {
    test("Concurrent access") {
      for {
        tokenStorageService <- ZIO.service[TokenStorageService]
        list <- ZIO.foreachPar(1 to 100)(_ => tokenStorageService.getOrCreateToken(1L))
      } yield assertTrue(list.distinct.size == 1)
    } @@ TestAspect.repeat(Schedule.recurs(10)) +
    test("the same token for the same user") {
      for {
        tokenStorageService <- ZIO.service[TokenStorageService]
        token1 <- tokenStorageService.getOrCreateToken(1)
        token2 <- tokenStorageService.getOrCreateToken(1)
      } yield assertTrue(token1 == token2)
    } +
    test("Token expiration") {
      for {
        tokenStorageService <- ZIO.service[TokenStorageService]
        token1 <- tokenStorageService.getOrCreateToken(1)
        _ <- TestClock.adjust(Duration.apply(2, TimeUnit.HOURS))
        token2 <- tokenStorageService.getOrCreateToken(1)
      } yield assertTrue(token2 != token1) && assertTrue(token2.expires.isAfter(token1.expires))
    }
  }.provide(ZLayer.fromZIO(TokenStorageService.apply())) +
  suite("checkExpired") {
    test("check expiration") {
      val v = for {
        tokenStorageService <- ZIO.service[TokenStorageService]
        token <- tokenStorageService.getOrCreateToken(1)
        _ <- TestClock.adjust(Duration.apply(2, TimeUnit.HOURS))
        result <- tokenStorageService.checkExpired(1, token.uid)
      } yield assertTrue(result)
      v
    } +
      test("check user not found") {
        val expired = for {
          tokenStorageService <- ZIO.service[TokenStorageService]
          token <- tokenStorageService.getOrCreateToken(1)
          result <- tokenStorageService.checkExpired(2, token.uid)
        } yield result
        assertZIO(expired.exit)(fails(equalTo(UserNotFoundError(2))))
      }
  }.provide(ZLayer.fromZIO(TokenStorageService()))