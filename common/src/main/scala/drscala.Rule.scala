package drscala

sealed trait Rule { def name: String }

object Rule {
  trait Sementic extends Rule { def rule(ctx: Sementic.Context): ctx.Apply }

  object Sementic {
    trait Context {
      val universe: Universe
      import universe.{Position => _, _}

      type Body = Tree
      type Apply = PhaseId => Check
      type Check = Body => Seq[(Position, Message)] 
    }
  }

  trait Style extends Rule { def rule: Seq[(String, Column => Position)] => Seq[(Position, Message)] }
}
