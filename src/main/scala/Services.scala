import zio._

trait TokenStorageService:
  def getOrCreateToken(userId: Long): ZIO[Any, Nothing, Token]
  def checkExpired(userId: Long, token: String): ZIO[Any, UserNotFoundError | AccessDenied, Boolean]

trait ContentService:
  def accessContent(contentId: Long): ZIO[Any, NoContentError, Content]


trait BusinessService:
  def issueToken(userId: Long): ZIO[TokenStorageService, Nothing, Token]
  def checkToken(userId: Long, token: String): ZIO[TokenStorageService, UserNotFoundError | AccessDenied, Boolean]
  def accessContent(userId: Long, token: String, contentId: Long): ZIO[TokenStorageService & ContentService, AccessDenied | UserNotFoundError | NoContentError, Content]


object BusinessServiceLive extends BusinessService:
  override def checkToken(userId: Long, token: String): ZIO[TokenStorageService, UserNotFoundError | AccessDenied, Boolean] =
    for {
      tokenStorageService <- ZIO.service[TokenStorageService]
      check <- tokenStorageService.checkExpired(userId, token)
    } yield check

  override def issueToken(userId: Long): ZIO[TokenStorageService, Nothing, Token] = for {
    tokenStorageService <- ZIO.service[TokenStorageService]
    token <- tokenStorageService.getOrCreateToken(userId)
  } yield token

  override def accessContent(userId: Long, token: String, contentId: Long): ZIO[TokenStorageService & ContentService, AccessDenied | UserNotFoundError | NoContentError, Content] =
    for {
      tokenStorageService <- ZIO.service[TokenStorageService]
      contentService <- ZIO.service[ContentService]
      validToken <- tokenStorageService.checkExpired(userId, token)
      _ <- if (!validToken) ZIO.succeed(()) else ZIO.fail(AccessDenied())
      content <- contentService.accessContent(contentId)
    } yield content

object TokenStorageService:
  def apply(): ZIO[Any, Nothing, TokenStorageService] = for {
    mapRef    <- Ref.make(Map.empty[Long, Token])
    semaphore <- Semaphore.make(1)
  } yield new TokenStorageService:
    extension (r: Ref[Map[Long, Token]])
      def generateAndUpdateToken(userId: Long): ZIO[Any, Nothing, Token] = for {
        uuid <- Random.nextUUID
        time <- Clock.currentDateTime
        token = Token(uuid.toString, time.plusHours(1L))
        _ <- r.update(m => m + (userId -> token))
      } yield token

    override def getOrCreateToken(userId: Long): ZIO[Any, Nothing, Token] =
      semaphore.withPermit(getOrCreateTokenInternal(userId))

    private def getOrCreateTokenInternal(userId: Long): ZIO[Any, Nothing, Token] =
      for {
        map         <- mapRef.get
        oldToken    =  map.get(userId)
        actualToken <- oldToken match {
          case Some(token) => for {
            currentTime  <- Clock.currentDateTime
            updatedToken <- if (token.expires.isBefore(currentTime)) mapRef.generateAndUpdateToken(userId)
            else ZIO.succeed(token)
          } yield updatedToken
          case None => mapRef.generateAndUpdateToken(userId)
        }
      } yield actualToken

    override def checkExpired(userId: Long, token: String): ZIO[Any, UserNotFoundError | AccessDenied, Boolean] = for {
      map <- mapRef.get
      storedToken = map.get(userId)
      now <- Clock.currentDateTime
      result <- storedToken match {
        case Some(t) if t.uid == token => ZIO.succeed(t.expires.isBefore(now))
        case Some(_) => ZIO.fail(AccessDenied())
        case None => ZIO.fail(UserNotFoundError(userId))
      }
    } yield result

object ContentServiceLive extends ContentService:
  override def accessContent(contentId: Long): ZIO[Any, NoContentError, Content] =
    if (contentId % 2 == 0) ZIO.succeed(s"Content $contentId")
    else ZIO.fail(NoContentError(contentId))