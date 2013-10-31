package drscala

import scala.Function._
import scala.tools.nsc.{Global, Phase}

// Note: Dear Cake, I hate you.
trait HealthCake {
  val global: Global

  import global._

  type Column = Int
  type Line = Int
  type PhaseId = String
  type Position = (Line, Column)
  type Message = String

  sealed abstract class Doctor {

    protected implicit def t2p(tuple: (Tree, Message)) = {
      val (tree, body) = tuple
      (tree.pos.line, tree.pos.column) -> body
    }

    def name: String
    def diagnostic: PartialFunction[PhaseId, CompilationUnit => Seq[(Position, Message)]] = PartialFunction.empty
    def examine: Seq[(String, Column => Position)] => Seq[(Position, Message)] = const(Nil)(_)
  }

  object Doctor {
    class StdLib extends Doctor {
      def name = "std-lib"

      val unsafeOnEmptyIterable = Seq("head", "last", "reduce", "reduceLeft", "reduceRight")

      def isNothingInferred(tree: Tree) = PartialFunction.cond(tree) {
        case ValDef(_, _, tpt, _) if tpt.exists(_.tpe =:= typeOf[Nothing]) => true
        case DefDef(_, _, _, _, tpt, _) if tpt.exists(_.tpe =:= typeOf[Nothing]) => true
      }

      override val diagnostic: PartialFunction[PhaseId, CompilationUnit => Seq[(Position, Message)]] = {
        case "parser" => _.body.collect {
          case tree@Ident(name) if name.toString == "$qmark$qmark$qmark" => 
            tree -> "Oops, an implementation is missing here."

          case tree@Apply(Ident(name), _) if name.toString == "println" => 
            tree -> "There is rarely a good reason to use `println`, is it the case here?"

          case tree@Select(_, name) if name.toString == "asInstanceOf" => 
            tree -> "There should be a better way than using `asInstanceOf`, what do you think?"
        }
        case "typer" => _.body.collect {
          case tree if isNothingInferred(tree) =>
            tree -> "I feel a disturbance in the force, the type `Nothing` might have been inferred."

          case tree@Select(value, name) if unsafeOnEmptyIterable.contains(name.toString) && value.tpe <:< typeOf[Iterable[Any]] => 
            tree -> (
              s"Are you sure the `${value.tpe.typeSymbol.name}` will never be empty?\n" +
              s"Because calling `$name` might throw an exception in this case."
            )

          case tree@Select(value, name) if name.toString == "get" && value.tpe <:< typeOf[Option[Any]] => 
            tree -> "There is surely a better way than calling `Option.get`, any idea?"
        }
      }

      override val examine: Seq[(String, Column => Position)] => Seq[(Position, Message)] = xs => {
        def emptyLines(lines: Seq[(String, Column => Position)]): Seq[(Position, Message)] = {
          val (count, xs) = lines.foldLeft((0, Seq.empty[Position])) { case ((count, xs), (line, pos)) =>
            if (line.trim.isEmpty) (count + 1) ->  xs
            else 0 -> { if (count > 1) (pos(1) +: xs ) else xs }
          }
          xs.map { case (line, column) => (line - 1, column) -> "Are these extra empty lines really needed?" }
        }

        xs.collect { 
          case (code, pos) if code.trim.endsWith(";") =>
            pos(code.length) -> "That `;` at the end of the line is unnecessary."
        } ++ emptyLines(xs)
      }
    }
  }
}

