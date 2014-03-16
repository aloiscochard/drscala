package drscala
package doctors

import org.brianmckenna.wartremover.{WartTraverser, WartUniverse}

trait WartComponent { self: HealthCake =>
  import self.global.{Position => _, _}
  import PhaseId._

  val IsDisabled = new Suffix(" is disabled")

  def format(message: String): String = message match {
    case IsDisabled(what) => s"Couldn't you avoid using `$what` here?"
    case message => message
  }

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
              def error(pos: RPosition, message: String) = xs = (pos.line, pos.column) -> format(message) :: xs
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
        .map(x => x.getClass.getSimpleName.toLowerCase.reverse.tail.reverse -> x).toMap

    def apply(names: Seq[String]): WartDoctor = new Doctor.Sementic("warts") with WartDoctor {
      val traversers: Seq[WartTraverser] = names.flatMap(warts.get)
    }

    def fromExp(exp: Selection.Exp[String]): WartDoctor = 
      apply(exp.map(_.toLowerCase).apply(warts.keys.toSeq))
  }
}
