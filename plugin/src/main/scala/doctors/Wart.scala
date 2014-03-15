package drscala
package doctors

import org.brianmckenna.wartremover.{WartTraverser, WartUniverse}

trait WartComponent { self: HealthCake =>
  import self.global._
  import PhaseId._

  trait WartDoctor extends Doctor.Sementic {
    def traversers: Seq[WartTraverser]

    override def apply = {
      case Typer => unit => 
        var xs: List[(Position, Message)] = Nil
        traversers.foreach { traverser =>
          traverser(
            new WartUniverse {
              import scala.reflect.internal.util.{Position => RPosition}
              val universe: global.type = self.global
              def error(pos: RPosition, message: String) = xs = (pos.line, pos.column) -> message :: xs
              def warning(pos: RPosition, message: String) = error(pos, message)
            }
          )(unit.body)
        }
        xs
      case _ => Nil.const
    }
  }

  object WartDoctor {
    val warts: Map[String, WartTraverser] = 
      PackageObjects[WartTraverser]("org.brianmckenna.wartremover.warts")
        .map(x => x.getClass.getSimpleName.toLowerCase -> x).toMap

    def apply(names: Seq[String]): WartDoctor = new Doctor.Sementic("warts") with WartDoctor {
      val traversers: Seq[WartTraverser] = names.flatMap(warts.get)
    }

    // TODO Introduce smart generic format for opt-in/opt-out
    def fromString(names: String): WartDoctor = apply(names.split("\\+").map(_.toLowerCase))
  }
}
