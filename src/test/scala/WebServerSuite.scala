import zio._
import zio.test._
import zio.http._
import Main.routes
import zio.json._

def baseRequest(): ZIO[Server, Nothing, Request] =
  ZIO.serviceWith[Server](_.port)
    .map(port => Request.get(url = URL.root.port(port)))

extension(testServer: TestServer)
  def addRoutes[R](
                    routes: Routes[R, Response],
                  ): ZIO[R, Nothing, Unit] =
    for {
      r <- ZIO.environment[R]
      provided          = routes.provideEnvironment(r)
      app: HttpApp[Any] = provided.toHttpApp
      _ <- testServer.driver.addApp(app, r)
    } yield ()

object WebServerSuite extends ZIOSpecDefault:
  def spec = suite("WebServerTest") {
    suite("NonEnvironmental") {
      test("FrameworkTest") {
        ZIO.succeed(assertTrue(true))
      }
    } +
      suite("Environmental") {
        test("TestServerTest") {
          for {
            client <- ZIO.service[Client]
            testRequest <- baseRequest()
            _ <- TestServer.addRequestResponse(testRequest, Response(Status.Ok))
            response <- client(testRequest)
          } yield assertTrue(response.status == Status.Ok)
        } +
          test("HealthRequestTest") {
            for {
              _ <- TestRandom.setSeed(123L)
              testRequest <- baseRequest()
              server <- ZIO.service[TestServer]
              client <- ZIO.service[Client]
              _ <- server.addRoutes(routes)
              response <- client(Request.get(testRequest.url / "health"))
              body <- response.body.asString
            } yield assertTrue(body == Health("ok", -535098017).toJson)
          }
      }.provide(
        TestServer.layer,
        Client.default,
        Scope.default,
        Driver.default,
        ZLayer.succeed(Server.Config.default.onAnyOpenPort),
        ZLayer.succeed(HealthServiceLive),
        ZLayer.succeed(BusinessServiceLive),
        ZLayer.succeed(new TokenStorageService {
          override def getOrCreateToken(userId: Long): ZIO[Any, Nothing, Token] = ???
          override def checkExpired(userId: Long, token: _root_.java.lang.String): ZIO[Any, UserNotFoundError, Boolean] = ???
        }),
        ZLayer.succeed(new ContentService {
          override def accessContent(contentId: Long): ZIO[Any, NoContentError, Content] = ???
        })
      )
  }