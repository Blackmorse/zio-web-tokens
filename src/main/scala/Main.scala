import zio._
import zio.http._
import zio.json.EncoderOps


object Main extends ZIOAppDefault:
  extension (r: Request)
    def parseLongQueryParam(paramName: String): ZIO[Any, ParseError, Long] = parseQueryParam(paramName, _.toLong)
  
    def parseStringQueryParam(paramName: String): ZIO[Any, ParseError, String] = parseQueryParam(paramName, identity)

    private def parseQueryParam[T](paramName: String, converter: String => T): ZIO[Any, ParseError, T] =
      ZIO.attempt(converter(r.url.queryParams.get(paramName).get))
        .mapError(e => ParseError(e.getMessage))

  private def issueToken(request: Request): ZIO[TokenStorageService & BusinessService, ParseError, Response] = for {
    userId <- request.parseLongQueryParam("userId")
    businessService <- ZIO.service[BusinessService]
    token <- businessService.issueToken(userId)
  } yield Response.text(token.toJson)

  private def checkTokenRequest(request: Request): ZIO[TokenStorageService with BusinessService, UserNotFoundError | ParseError | AccessDenied, Response] = for {
    userId <- request.parseLongQueryParam("userId")
    token <- request.parseStringQueryParam("token")
    businessService <- ZIO.service[BusinessService]
    check <- businessService.checkToken(userId, token)
  } yield Response.text(check.toString)

  def accessContentRequest(request: Request): ZIO[TokenStorageService & ContentService with BusinessService, ParseError | AccessDenied | UserNotFoundError | NoContentError, Response] = for {
    userId <- request.parseLongQueryParam("userId")
    token <- request.parseStringQueryParam("token")
    contentId <- request.parseLongQueryParam("contentId")
    businessService <- ZIO.service[BusinessService]
    content <- businessService.accessContent(userId, token, contentId)
  } yield Response.text(content)
  
  val routes = Routes(
    Method.GET / "health" -> handler {
      ZIO.service[HealthService]
        .flatMap(_.health)
        .map(health => Response.json(health.toJson))
    },
    Method.POST / "issueToken" -> handler { (request: Request) =>
      issueToken(request)
        .catchAll(parseError => ZIO.succeed(Response.badRequest(parseError.text)))
    },
    Method.GET / "checkToken" -> handler { (request: Request) =>
      checkTokenRequest(request)
        .catchAll {
          case e: UserNotFoundError => ZIO.succeed(Response.forbidden(s"No such user: ${e.userId.toString}"))
          case e: ParseError => ZIO.succeed(Response.badRequest(e.text))
        }
    },
    Method.GET / "content" -> handler { (request: Request) =>
      accessContentRequest(request)
        .catchAll {
          case e: ParseError => ZIO.succeed(Response.badRequest(e.text))
          case e: AccessDenied => ZIO.succeed(Response.forbidden(s"Token is not valid"))
          case e: UserNotFoundError => ZIO.succeed(Response.forbidden(s"No such user: ${e.userId.toString}"))
          case e: NoContentError => ZIO.succeed(Response.notFound(s"No content with id ${e.contentId}"))
        }
    })

  val apps: HttpApp[TokenStorageService & ContentService & BusinessService & HealthService] = routes.toHttpApp

  def run = Server.install(apps)
    .flatMap { port =>
      Console.printLine(s"Started on $port") *> ZIO.never
    }
    .provide(Server.defaultWithPort(8888),
      ZLayer.succeed(HealthServiceLive),
      ZLayer.succeed(BusinessServiceLive),
      ZLayer.fromZIO(TokenStorageService()),
      ZLayer.succeed(ContentServiceLive)
    )