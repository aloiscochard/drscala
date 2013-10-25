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
      val diagnostic: PartialFunction[PhaseId, CompilationUnit => Seq[(Position, Message)]] = {
        case "parser" => _.body.collect {
          case tree@Apply(Select(_, name), _) if name.toString == "asInstanceOf" => 
            tree.pos -> "There should be a better way than using `asInstanceOf`, what do you think?"
        }
      }
    }
  }
}

