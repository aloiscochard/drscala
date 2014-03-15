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
  case object Typer extends PhaseId("typer")
  case object Parser extends PhaseId("parser")

  object PhaseId {
    def values = Set(Typer, Parser)
    def fromString(name: String): Option[PhaseId] = values.find(_.name == name)
  }

  abstract class Doctor {

    protected implicit def t2p(tuple: (Tree, Message)) = { val (tree, body) = tuple; (tree.pos.line, tree.pos.column) -> body }

    def name: String
    def diagnostic: PhaseId => CompilationUnit => Seq[(Position, Message)]
    def examine: Seq[(String, Column => Position)] => Seq[(Position, Message)] = const(Nil)(_)
  }
}

