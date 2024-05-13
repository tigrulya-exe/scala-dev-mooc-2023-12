package AkkaDataStreams
import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior, DeathPactException, SupervisorStrategy}

object Protocol {
  sealed trait Command
  case class Fail(text: String) extends Command
  case class Hello(text: String) extends  Command
}

import Protocol._

object Worker {
  def apply(): Behavior[Command] =
    Behaviors.receiveMessage{
      case Fail(text) =>
        throw new RuntimeException(text)
      case Hello(text) =>
        println(text)
        Behaviors.same
    }

}

object MiddleManager{
  def apply(): Behavior[Command] =
    Behaviors.setup[Command]{ctx =>
      ctx.log.info("MiddleManager starting up")
      val child = ctx.spawn(Worker(), "worker")
      ctx.watch(child)

      Behaviors.receiveMessage{msg =>
        child ! msg
        Behaviors.same
      }
    }
}

object Boss{
  def apply(): Behavior[Command] = {
    Behaviors.supervise(
    Behaviors.setup[Command]{ctx =>
      ctx.log.info("Boss starting up")
      val midlManager = ctx.spawn(MiddleManager(), "MiddleManager")
      ctx.watch(midlManager)
      Behaviors.receiveMessage[Command]{msg =>
        midlManager ! msg
        Behaviors.same
      }
    }).onFailure[DeathPactException](SupervisorStrategy.restart)
  }


}

object StartStopSpec1 extends App {
  def apply(): Behavior[NotUsed] =
    Behaviors.setup{ctx =>
      val boss = ctx.spawn(Boss(), "upper-management")
      boss ! Hello("hi 1")
      boss.tell(Fail("ping"))
      Thread.sleep(1000)
      boss ! Hello("hi 2")
      Behaviors.same
    }

  val value = StartStopSpec1()
  implicit val system = ActorSystem(value, "akka_typed")
  Thread.sleep(5000)
  system.terminate()
}