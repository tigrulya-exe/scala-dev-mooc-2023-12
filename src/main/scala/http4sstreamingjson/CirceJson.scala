package http4sstreamingjson

import cats.effect.{IO, IOApp}
import io.circe.Decoder
import io.circe.derivation.deriveDecoder
import io.circe.parser.parse
import org.http4s.Method.POST
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.{HttpRoutes, Request, Uri}
//import io.circe.generic.semiauto._
import io.circe.generic.auto._
//import io.circe.derivation._
import com.comcast.ip4s.{Host, Port}
import org.http4s.Method
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router


object CirceJson extends IOApp.Simple {
  case class User(name: String, email: Option[String])
  case class Permissions(user: User, id:Int)

  //1. decoder for the user
/*  implicit val decoderUser: Decoder[User] = Decoder.instance(
    cur =>
      for {
        name <- cur.downField("name").as[String]
        email <- cur.downField("email").as[Option[String]]
      } yield User(name, email)
  )*/

  //2. semiauto
  implicit val decoderUser: Decoder[User] = deriveDecoder

  val example = """{"name": "1111", "email": "1111@2222.de"}"""
//  val json: Either[ParsingFailure, Json] = parse(example)

  def run: IO[Unit] = IO.println{
    for {
      json <- parse(example)
      user <- json.as[User]
    } yield user
  }
}

import org.http4s.circe.CirceEntityDecoder._
import org.http4s.circe.CirceEntityEncoder._

object restfulldesc {
  def publicRoutes: HttpRoutes[IO] = HttpRoutes.of {
    case r@POST -> Root / "echo" =>
      for {
        u <- r.as[CirceJson.User]
        _ <- IO.println(u)
        response <- Ok(u)
      } yield response
  }

  val router = Router("/public" -> publicRoutes)
  val server = for {
    s <- EmberServerBuilder
      .default[IO]
      .withPort(Port.fromInt(8080).get)
      .withHost(Host.fromString("localhost").get)
      .withHttpApp(router.orNotFound).build
  } yield s
}

object HttpClientCircy {
  val builder = EmberClientBuilder.default[IO].build
  val postrequest = Request[IO](
    method = Method.POST,
    uri = Uri.fromString("http://localhost:8080/public/echo").toOption.get)
    .withEntity(CirceJson.User("aaesfwef", Some("asasf@afwef.de")))

  val result = builder.use(
    client => client.run(postrequest).use(
      resp =>
        if (resp.status.isSuccess)
          resp.as[CirceJson.User]
        else
          IO.raiseError(new Exception("error"))
    )
  )
}

object MainCircePars extends IOApp.Simple {
  def run: IO[Unit] = for {
    _ <- restfulldesc.server.use(_ =>
      HttpClientCircy.result.flatMap(IO.println) *> IO.never
    )
  } yield()
}