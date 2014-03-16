package drscala
package rules

import PhaseId._
import Random._

object StdRules extends RuleSet("std") with RuleSet.DSL {
  override def rules = {
    Phase(Parser) { phase => import phase._
      Seq(
        CheckSementic("TripleQMark")(ctx => new TreeCheck(ctx) { import universe._
          def apply = _.collect { 
            case tree@Ident(name) if name.toString == "$qmark$qmark$qmark" => 
              tree.signal("Oops, an implementation is missing here.")
          }
        }),
        CheckSementic("println")(ctx => new TreeCheck(ctx) { import universe._
          def apply = _.collect { 
            case tree@Apply(Ident(name), _) if name.toString == "println" => 
              tree.signal("There is rarely a good reason to use `println`, is it the case here?")
          }
        }),
        CheckSementic("asInstanceOf")(ctx => new TreeCheck(ctx) { import universe._
          def apply = _.collect { 
            case tree@Select(_, name) if name.toString == "asInstanceOf" => 
              tree.signal("There should be a better way than using `asInstanceOf`, what do you think?")
          }
        })
      )
    } ++
    Phase(Typer) { phase => import phase._
      Seq(
        CheckSementic("Nothing")(ctx => new TreeCheck(ctx) { import universe._
          def isNothingInferred(tree: Tree) = PartialFunction.cond(tree) {
            case ValDef(_, _, tpt, _) if tpt.exists(_.tpe =:= typeOf[Nothing]) => true
            case DefDef(_, _, _, _, tpt, _) if tpt.exists(_.tpe =:= typeOf[Nothing]) => true
          }

          def apply = _.collect { case tree if isNothingInferred(tree) =>
            tree.signal("I feel a disturbance in the force, the type `Nothing` might have been inferred.")
          }
        }),
        CheckSementic("UnsafeIterable")(ctx => new TreeCheck(ctx) { import universe._
          val unsafeOnEmptyIterable = Seq("head", "last", "reduce", "reduceLeft", "reduceRight")

          def apply = _.collect { 
            case tree@Select(value, name) if unsafeOnEmptyIterable.contains(name.toString) && value.tpe <:< typeOf[Iterable[Any]] => 
              tree.signal(
                s"Are you sure the `${value.tpe.typeSymbol.name}` will never be empty?\n" +
                s"Because calling `$name` might throw an exception in this case."
              )
          }
        }),
        CheckSementic("UnsafeOption")(ctx => new TreeCheck(ctx) { import universe._
          def apply = _.collect { 
            case tree@Select(value, name) if name.toString == "get" && value.tpe <:< typeOf[Option[Any]] => 
              tree.signal("There is surely a better way than calling `Option.get`, any idea?")
          }
        })
      )
    } ++
    Seq(
      StyleCheck("EmptyLines") { lines =>
        def aBigAnimal = ofTwo("an :elephant:", "a :whale:")
        val (count, xs) = lines.foldLeft((0, Seq.empty[Position])) { case ((count, xs), (line, pos)) =>
          if (line.trim.isEmpty) (count + 1) ->  xs
          else 0 -> { if (count > 1) (pos(1) +: xs ) else xs }
        }
        xs.map { case (line, column) => (line - 1, column) -> (
          s"There is enough room for $aBigAnimal!\n" +
          "Are these extra empty lines really needed?"
        )}
      },
      StyleCheck("Semicolons")(_.collect {
        case (code, pos) if code.trim.endsWith(";") =>
          pos(code.length) -> "That `;` at the end of the line is unnecessary."
      })
    )
  }
}
