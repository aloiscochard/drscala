package drscala

import scala.Function._
import scala.tools.nsc.{Global, Phase}

// Note: Dear Cake, I hate you.
trait HealthCake {
  val global: Global

  import global.{Position => _, _}

  sealed trait Doctor { 
    def name: String
  }

  object Doctor {
    abstract class Sementic(val name: String) extends Doctor {
      protected implicit def t2p(tuple: (Tree, Message)) = { val (tree, body) = tuple; (tree.pos.line, tree.pos.column) -> body }
      def apply: PhaseId => CompilationUnit => Seq[(Position, Message)]
    }

    abstract class Style(val name: String) extends Doctor {
      def apply: Seq[(String, Column => Position)] => Seq[(Position, Message)] = const(Nil)(_)
    }
  }
}

