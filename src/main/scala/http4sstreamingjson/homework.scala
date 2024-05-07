package http4sstreamingjson

import cats.effect.kernel.{Ref, Resource}
import cats.effect.{IO, IOApp}
import cats.implicits.{toBifunctorOps, toSemigroupKOps}
import com.comcast.ip4s.{Host, Port}
import fs2.Stream
import io.circe.Encoder
import io.circe.derivation.deriveEncoder
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Server
import org.http4s.{Http, HttpRoutes}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

object HomeworkServer {

  case class CounterWrapper(value: Long)

  implicit val counterEncoder: Encoder[CounterWrapper] = deriveEncoder

  type Session[F[_]] = Ref[F, Long]

  def counterService(session: Session[IO]): HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root / "counter" =>
        for {
          counter <- session.updateAndGet(_ + 1)
          response <- Ok(CounterWrapper(counter))
        } yield response
    }
  }

  val chunkProviderService: HttpRoutes[IO] = {
    HttpRoutes.of {
      case GET -> Root / "slow"
        / rawChunkSize
        / rawTotalSize
        / rawTime =>

        val streamOrError = for {
          chunkSize <- positiveOrError(rawChunkSize)
          totalSize <- positiveOrError(rawTotalSize)
          time <- positiveOrError(rawTime)
          stream = chunkStream(chunkSize, totalSize, time.seconds)
        } yield stream

        streamOrError match {
          case Left(errorMessage) => BadRequest(errorMessage)
          case Right(stream) => Ok(stream)
        }
    }
  }

  def httpApp(session: Session[IO]): Http[IO, IO] = (chunkProviderService <+> counterService(session)).orNotFound

  val server: Resource[IO, Server] = for {
    session <- Resource.eval(Ref[IO].of(0L))
    server <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8081).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(httpApp(session))
      .build
  } yield server

  private def chunkStream(chunkSize: Int, chunkTotalSize: Int, delay: FiniteDuration): Stream[IO, Byte] = {
    Stream
      .range(0, chunkTotalSize)
      .map(_ => 1.asInstanceOf[Byte])
      .chunkN(chunkSize, allowFewer = true)
      .covary[IO]
      .metered(delay)
      .unchunks
  }

  private def positiveOrError(value: => String): Either[String, Int] = {
    Try(value.toInt)
      .filter(_ >= 0)
      .toEither
      .leftMap(_ => s"Error parsing value: $value")
  }
}

object HomeworkServerApp extends IOApp.Simple {
  def run(): IO[Unit] = {
    HomeworkServer.server.use(_ => IO.never)
  }
}