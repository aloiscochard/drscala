package drscala

import scala.Function._
import scala.tools.nsc.{Global, Phase}

// Note: Dear Cake, I hate you.
trait HealthCake {
  val global: Global

  import global._

  type Column = Int
  type Line = Int
  type Position = (Line, Column)
  type Message = String

  sealed abstract class PhaseId(val name: String)

  object PhaseId {
    case object Typer extends PhaseId("typer")
    case object Parser extends PhaseId("parser")

    def values = Set(Typer, Parser)
    def fromString(name: String): Option[PhaseId] = values.find(_.name == name)
  }

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

