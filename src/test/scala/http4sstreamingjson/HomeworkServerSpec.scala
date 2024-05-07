package http4sstreamingjson

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import fs2.Stream
import http4sstreamingjson.HomeworkServer.{CounterWrapper, httpApp}
import io.circe.Decoder
import io.circe.derivation.deriveDecoder
import org.http4s.Request
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.client.Client
import org.http4s.implicits._
import org.scalatest.flatspec.AnyFlatSpec

class HomeworkServerSpec extends AnyFlatSpec {
  object HomeworkClient {
    implicit val counterDecoder: Decoder[CounterWrapper] = deriveDecoder

    private val client: IO[Client[IO]] = {
      for {
        session <- Ref[IO].of(0L)
        client = Client.fromHttpApp(httpApp(session))
      } yield client
    }

    private val getCounterRequest = Request[IO](uri = uri"/counter")

    def getCounter(executeTimes: Int = 1): IO[Long] = for {
      client <- client
      wrappers <- client.expect[CounterWrapper](getCounterRequest)
        .replicateA(executeTimes)
    } yield wrappers.last.value

    def getByteArray(chunkSize: Int, totalSize: Int, delaySec: Long): IO[Array[Byte]] = {
      val chunksStream = for {
        client <- Stream.eval(client)
        response <- client.stream(
          Request[IO](uri = uri"/slow" / chunkSize / totalSize / delaySec)
        )
        responseStream <- response.body
      } yield responseStream

      chunksStream.compile.to(Array)
    }
  }

  it should "increment counter 10 times" in {
    val lastCounterValue = HomeworkClient.getCounter(executeTimes = 10)

    assert(lastCounterValue.unsafeRunSync() == 10)
  }

  it should "return full stream of chunks" in {
    val bytes = HomeworkClient.getByteArray(
      chunkSize = 128,
      totalSize = 1025,
      delaySec = 1
    )

    val expectedResult = Array.fill(1025)(1.toByte)
    assert(bytes.unsafeRunSync() sameElements expectedResult )
  }
}
