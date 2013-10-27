package drscala

import scala.tools.nsc.{Global, Phase}

// Note: Dear Cake, I hate you.
trait HealthCake {
  val global: Global

  import global._

  type PhaseId = String
  type Message = String

  sealed abstract class Doctor {
    def name: String
    def diagnostic: PartialFunction[PhaseId, CompilationUnit => Seq[(Position, String)]]
  }

  object Doctor {
    class StdLib extends Doctor {
      def name = "std-lib"

      val unsafeOnEmptyIterable = Seq("head", "last", "reduce", "reduceLeft", "reduceRight")

      val diagnostic: PartialFunction[PhaseId, CompilationUnit => Seq[(Position, Message)]] = {
        case "parser" => _.body.collect {
          case tree@Select(_, name) if name.toString == "asInstanceOf" => 
            tree.pos -> "There should be a better way than using `asInstanceOf`, what do you think?"
        }
        case "typer" => unit => 
          unit.body.collect {
            case tree@Select(value, name) if unsafeOnEmptyIterable.contains(name.toString) && value.tpe <:< typeOf[Iterable[Any]] => 
              tree.pos -> (
                s"Are we sure the `${value.tpe.typeSymbol.name}` will never be empty?\n" +
                s"Because calling `$name` might throw an exception in this case."
              )
          }
      }
    }
  }
}

