package drscala

import Rule._

abstract class RuleSet(val name: String) {
  def rules: Seq[Rule] = Nil
}

object RuleSet {
  trait DSL { self: RuleSet =>

    class Phase(phase: PhaseId) { 
      type Context = Sementic.Context

      class CheckSementic(name: String, check: Context => TreeCheck) { self =>
        def build: Sementic = new Sementic {
          val name = self.name
          def rule(ctx: Sementic.Context): ctx.Apply = _ match {
            case `phase` => tree =>
              val x = check(ctx)
              // The Cake is a lie.
              x.apply(tree.asInstanceOf[x.universe.Tree])
            case _ => Nil.const
          }
        }
      }

      object CheckSementic { def apply(name: String)(check: Context => TreeCheck) = new CheckSementic(name, check).build }

      // TODO Make ctx implicit once scalac bug fixed
      abstract class TreeCheck(ctx: Context) extends Sementic.Context { 
        val universe = ctx.universe
        def apply: Check 
        import universe._
        implicit class TreeOps(tree: Tree) {
          def signal(m: String) = (tree.pos.line, tree.pos.column) -> m
        }
      }

    }

    object Phase { def apply(phase: PhaseId)(f: Phase => Seq[Sementic]): Seq[Sementic] = f(new Phase(phase)) }

    class StyleCheck(n: String) {
      def apply(f: Seq[(String, Column => Position)] => Seq[(Position, Message)]): Style = new Style {
        val name = n
        val rule = f
      }
    }

    object StyleCheck { def apply(name: String): StyleCheck = new StyleCheck(name) } 
  }
}
