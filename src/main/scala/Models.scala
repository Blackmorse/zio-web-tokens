import java.time.OffsetDateTime
import zio.json._

type Content = String

//Token.scala

case class Token(uid: String, expires: OffsetDateTime)

object Token:
  given JsonEncoder[Token] = DeriveJsonEncoder.gen[Token]

//Errors.scala

sealed trait AppError

case class UserNotFoundError(userId: Long) extends AppError
case class NoContentError(contentId: Long) extends AppError
case class AccessDenied() extends AppError
case class ParseError(text: String) extends AppError