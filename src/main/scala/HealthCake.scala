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

  abstract class Doctor {

    protected implicit def t2p(tuple: (Tree, Message)) = { val (tree, body) = tuple; (tree.pos.line, tree.pos.column) -> body }

    def name: String
    def diagnostic: PartialFunction[PhaseId, CompilationUnit => Seq[(Position, Message)]] = PartialFunction.empty
    def examine: Seq[(String, Column => Position)] => Seq[(Position, Message)] = const(Nil)(_)
  }
}

