package http4sstreamingjson

import org.http4s.ember.client.EmberClientBuilder
import catsmiddleware.Restfull
import cats.effect.{IO, IOApp, Resource}
import org.http4s.client.Client
import org.http4s.{Request,Response, Uri}
import cats.effect

object HttpClient {
  val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val request = Request[IO](uri = Uri.fromString("http://localhost:8080/hello").toOption.get)

  //1
  /*  val result: Resource[IO, Response[IO]] = for {
    client <- builder
    response <- client.run(request)
  } yield response*/
  //2
/*  val result: Resource[IO, String] = for {
    client <- builder
    response <- effect.Resource.eval(client.expect[String](request))
  } yield response
*/
  //3 endup with clear code
  val result = builder.use(
    client => client.run(request).use(
      resp =>
        if (!resp.status.isSuccess)
          resp.body.compile.to(Array).map(new String(_))
        else
          IO("Error")
    )
  )
}

object mainServer extends IOApp.Simple {
  def run(): IO[Unit] = {
    // for 1 and 2
/*    for {
      fiber <- Restfull.sesverSessionAuthServerClear.use(_ => IO.never).start
      _ <- HttpClient.result.use(IO.println)
      _ <- fiber.join
    } yield () */
    for {
      _ <- Restfull.sesverSessionAuthServerClear.use(_ => HttpClient.result.flatMap(IO.println) *> IO.never)
    } yield ()
  }
}