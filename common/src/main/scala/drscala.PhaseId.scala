package drscala

sealed abstract class PhaseId(val name: String)

object PhaseId {
  case object Typer extends PhaseId("typer")
  case object Parser extends PhaseId("parser")
  def values = Set(Typer, Parser)
  def fromString(name: String): Option[PhaseId] = values.find(_.name == name)
}

