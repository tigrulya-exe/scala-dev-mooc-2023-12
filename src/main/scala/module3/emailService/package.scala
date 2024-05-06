package module3

import zio.Has
import zio.{UIO, URIO}
import zio.{ULayer, ZLayer}
import zio.console
import zio.ZIO
import zio.console.Console
import zio.macros.accessible


package object emailService {

    /**
     * Реализовать Сервис с одним методом sendEmail,
     * который будет принимать Email и отправлять его
     */

     // 1
     type EmailService = Has[EmailService.Service]

     // 2
     object EmailService {
       trait Service{
         def sendEmail(email: Email): URIO[zio.console.Console, Unit]
       }


       //3
       val live = ZLayer.succeed(new Service {
         override def sendEmail(email: Email): URIO[Console, Unit] =
           zio.console.putStrLn(email.toString).orDie
       })

       def sendEmail(email: Email): URIO[EmailService with zio.console.Console, Unit] =
         ZIO.accessM[EmailService with Console](_.get.sendEmail(email))
     }


}
