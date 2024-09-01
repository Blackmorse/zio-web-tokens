import zio.json._
import zio._

case class Health(status: String, seed: Int)

object Health:
  given JsonEncoder[Health] = DeriveJsonEncoder.gen[Health]

trait HealthService:
  def health: ZIO[Any, Nothing, Health]

object HealthServiceLive extends HealthService:
  override def health: ZIO[Any, Nothing, Health] =
    Random.nextInt.map(n => Health("ok", n))