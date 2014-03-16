package drscala
package doctors

trait RuleComponent { self: HealthCake =>

  val ruleSets: Map[String, RuleSet] = PackageObjects[RuleSet]("drscala.rules").map(x => x.name.toLowerCase -> x).toMap

  object RuleContext extends Rule.Sementic.Context { val universe = self.global }

  object RuleDoctor {
    def fromRuleSet(rs: RuleSet): Selection.Exp[String] => Seq[Doctor] = exp => {
      val expLow = exp.map(_.toLowerCase)
      val rules = rs.rules.filter(x => expLow.contains(x.name.toLowerCase))

      val sementics = rules.collect { case rule: Rule.Sementic => rule }
      val styles = rules.collect { case rule: Rule.Style => rule }

      val sementic = sementics match {
        case Nil => None
        case xs => Some(new Doctor.Sementic(rs.name) {
          override def apply = phase => unit =>
            // The Cake is a lie.
            xs.flatMap(_.rule(RuleContext)(phase)(unit.body.asInstanceOf[RuleContext.universe.Tree]))
        })
      }

      val style = styles match {
        case Nil => None
        case xs => Some(new Doctor.Style(rs.name) {
          override def apply = lines => xs.flatMap(_.rule(lines))
        })
      }

      sementic.toSeq ++ style.toSeq
    }
  }
}

