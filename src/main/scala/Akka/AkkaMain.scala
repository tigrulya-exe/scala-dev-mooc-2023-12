package Akka

import Akka.AkkaMain3.change_behaviour
import Akka.AkkaMain3.change_behaviour.WorkerProtocol
import Akka.AkkaMain4.handle_state.Counter
import Akka.AkkaMain4.handle_state.Counter.CounterProtocol
import Akka.AkkaMain4.handle_state.Counter.CounterProtocol.Inc
import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Props, SpawnProtocol}
import akka.util.Timeout
import akka.actor.typed.scaladsl.AskPattern._

import scala.language.{existentials, postfixOps}
import scala.concurrent.Future
import scala.concurrent.duration._



object AkkaMain {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem[String](intro_akka.behaviour_factory_methods.Echo(), "Echo")
    system ! "Hello"
    Thread.sleep(3000)
    system.terminate()
  }
}


object AkkaMain2{
  object Supervisor {
    def apply(): Behavior[SpawnProtocol.Command] = Behaviors.setup{ctx =>
      ctx.log.info(ctx.self.toString)
      SpawnProtocol()
    }
  }

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem[SpawnProtocol.Command](Supervisor(), "Echo")
    implicit val ec = system.executionContext
    implicit val timeout = Timeout(3 seconds)
    val echo: Future[ActorRef[String]] = system.ask(
      SpawnProtocol.Spawn(intro_akka.behaviour_factory_methods.Echo(), "Echo", Props.empty, _)
    )

    for (ref <- echo)
      ref ! "Hello from ask"
  }
}

// 3. change state
object AkkaMain3 {
  object change_behaviour {
    sealed trait  WorkerProtocol
    object WorkerProtocol {
      case object Start extends  WorkerProtocol
      case object StandBy extends WorkerProtocol
      case object Stop extends WorkerProtocol
    }

    import WorkerProtocol._
    def apply(): Behavior[WorkerProtocol] = idle()
    def idle(): Behavior[WorkerProtocol] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case msg@Start =>
          ctx.log.info(msg.toString)
          workInProgress()
        case msg@StandBy =>
          ctx.log.info(msg.toString)
          idle()
        case msg@Stop =>
          ctx.log.info(msg.toString)
          Behaviors.stopped
      }
    }

    def workInProgress(): Behavior[WorkerProtocol] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case msg@Start => Behaviors.unhandled
        case msg@StandBy =>
          ctx.log.info("go to standBy")
          idle()
        case msg@Stop =>
          ctx.log.info("stopped")
          Behaviors.stopped
      }
    }
  }

  def main(args: Array[String]): Unit ={
    val system = ActorSystem[WorkerProtocol](change_behaviour(), "Echo")
    system ! WorkerProtocol.Start
    Thread.sleep(1000)
    system ! WorkerProtocol.StandBy
    Thread.sleep(1000)
    system ! WorkerProtocol.Stop
    Thread.sleep(1000)
    system.terminate()

  }
}

//4. state with calculations
object AkkaMain4 {
  object handle_state {
    object Counter {
      sealed trait CounterProtocol

      object CounterProtocol {
        final case object Inc extends CounterProtocol

        final case class GetCounter(replyTo: ActorRef[Int]) extends CounterProtocol
      }

      import CounterProtocol._

      def apply(init: Int): Behavior[CounterProtocol] = inc(init)

      def inc(counter: Int): Behavior[CounterProtocol] = Behaviors.setup { ctx =>
        Behaviors.receiveMessage {
          case Inc =>
            ctx.log.info(s"counter $counter")
            inc(counter + 1)
          case GetCounter(replyTo) =>
            replyTo ! counter
            Behaviors.same
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem[CounterProtocol](Counter(0), "Echo")
    system ! Inc
    system ! Inc
    system ! Inc
    Thread.sleep(1000)
    system.terminate()
  }
}

//disp.
object Dispatcher extends App {
  object task_dispatcher {

    object LogWorker {}

    object ParseUrlWorker {}

  }

  def apply(): Behavior[NotUsed] = ???

  implicit  val system = ActorSystem(Dispatcher(), "sfeswegf")

}