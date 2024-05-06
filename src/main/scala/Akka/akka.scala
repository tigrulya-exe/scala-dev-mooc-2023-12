package Akka

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

object intro_akka {
  //1. functional style
  object behaviour_factory_methods {
    object Echo {
      def apply(): Behavior[String] = Behaviors.setup{ctx =>
        Behaviors.receiveMessage{
          case msg =>
            ctx.log.info(msg)
            Behaviors.same
        }
      }
    }
  }

  object abstract_behaviour {
    class Echo(ctx: ActorContext[String]) extends AbstractBehavior[String](ctx) {
      override def onMessage(msg: String): Behavior[String] = {
        ctx.log.info(msg)
        this
      }
    }

    object Echo {
      def apply(): Behavior[String] = Behaviors.setup{ctx =>
        new Echo(ctx)
      }
    }
  }


}
