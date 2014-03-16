package drscala

import scala.PartialFunction.cond
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception._
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

import github.{Client, Credentials, RepositoryId}
import doctors._
import Selection._

class CompilerPlugin(val global: Global) extends Plugin with HealthCake 
    with RuleComponent with WartComponent { import global.{Lazy => _, Position => _,  _}

  implicit val ec: ExecutionContext = ExecutionContext.global

  trait Checkup extends PluginComponent {
    import global.{Position => _, _}

    def checkup: CompilerPlugin.this.global.CompilationUnit => Seq[(Position, String)]

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        lazy val writer = reporterOption.flatMap { reporter =>
          reporter.scope.view.flatMap { case (_, xs) => 
            xs.find { case (filename, _) => unit.source.file.path.endsWith(filename) }
              .map { case (filename, _) => reporter(filename) }
          }.headOption
        }

        trace(s"writer@${unit.source.file.path}=$writer")
        if (Settings.warn || writer.isDefined) {
          // TODO Find a way to avoid casting CompilationUnit due to cake madness.
          val comments = checkup(unit.asInstanceOf[CompilerPlugin.this.global.CompilationUnit])

          trace(comments.mkString("\n"))

          if (Settings.warn) comments.foreach { case ((line, column), body) => 
            unit.warning(unit.source.position(line - 1, column - 1), s"$phaseName\n$body")
          }

          writer.foreach(_(comments.map { case ((line, column), body) => Reporter.Report(body, column, line)}))
        }
      }
    }
  }

  class CheckupSementic(phase: PhaseId, doctors: Lazy[Seq[Doctor.Sementic]], val global: Global = CompilerPlugin.this.global) extends Checkup {
    override val runsAfter = List(phase.name)
    override val phaseName = s"drscala.${phase}"

    val checkup = (unit: CompilationUnit) => doctors.value.flatMap(_.apply(phase)(unit))
  }

  class CheckupExamine(doctors: Lazy[Seq[Doctor.Style]], val global: Global = CompilerPlugin.this.global) extends Checkup { 
    override val runsAfter = List("namer")
    override val phaseName = "drscala.examine"

    val eof: (String, Column => Position) =  ("EOF", _ => (0, 0))

    val checkup = (unit: CompilationUnit) => {
      // TODO Optimisation: when processing PR, check only part which are in the diff.
      val xs = new String(unit.source.file.toByteArray).split("\n")
        .zipWithIndex.map { case (k, v) => k -> ((x: Int) => (v + 1, x)) }
      doctors.value.flatMap(_.apply(xs :+ eof))
    }
  }

  val active = ! Settings.disabled
  val name = "drscala"
  val description = "A doctor for your code"

  val doctors = Lazy {
    def rules = Settings.rules.nonEmpty.map { actives =>
      val xs = ruleSets.filterKeys(actives.contains)
      ruleSets.flatMap { case (name, rs) => 
        RuleDoctor.fromRuleSet(rs)(Settings.ruleSets.getOrElse(name, Exp.All())) 
      }
    }.getOrElse(Nil)

    def warts = Settings.warts.nonEmpty.map(WartDoctor.fromExp).toSeq

    (rules ++ warts).toSeq
  }

  val sementics = Lazy(doctors.value.collect { case x: Doctor.Sementic => x})
  val styles = Lazy(doctors.value.collect { case x: Doctor.Style => x})

  val components = 
    if (active) 
      new CheckupExamine(styles) :: PhaseId.values.map(new CheckupSementic(_, sementics)).toList
    else Nil

  object Settings {
    case class GitHub(credentials: Credentials, repositoryId: RepositoryId) {
      lazy val pullRequestId = GitHub.pullRequestId
    }

    object GitHub { 
      def pullRequestId = 
        Option(System.getProperty("drscala.pr"))
          .orElse(Option(System.getenv("DRSCALA_PR")))
          .orElse(Option(System.getenv("ghprbPullId")))
          .map(_.toInt)

      val User = new Prefix("gh.user="); val Password = new Prefix("gh.password=")
      val RepositoryOwner = new Prefix("gh.repository.owner="); val RepositoryName = new Prefix("gh.repository.name=")
    }

    val Rules = new Prefix("rules=")
    val Warts = new Prefix("warts=")

    object RuleSet {
      val prefix = new Prefix("rules.")

      def unapply(s: String): Option[(String, String)] = prefix.unapply(s).flatMap { s =>
        s.split("=") match {
          case Array(set, exp) => Some(set -> exp)
          case _ => None
        }
      }
    }

    var debug = false

    val disabled = Option(System.getProperty("drscala.disable"))
      .orElse(Option(System.getenv("DRSCALA_DISABLE")))
      .fold(false)(_.toLowerCase == "true")

    var github: Option[GitHub] = None

    var rules: Exp[String] = Exp.All()
    var ruleSets: Map[String, Exp[String]] = Map.empty
    var warts: Exp[String] = Exp.include(Seq("Any2StringAdd", "Null", "Var"))

    var warn = false
  }

  private var reporterOption: Option[Reporter] = None

  private def trace(message: => String): Unit = if (Settings.debug) { println(message) }

  private def reporterInit: Option[Reporter] = Settings.github.flatMap { github =>
    github.pullRequestId.flatMap { prId =>
      allCatch.either {
        import scala.concurrent.Await
        import scala.concurrent.duration._
        Await.result(Reporter(github.credentials, github.repositoryId, prId), Duration.Inf)
      }.left.map { throwable =>
        throwable.printStackTrace()
        throwable
      }.right.toOption
    }
  }

  override def processOptions(options: List[String], error: String => Unit) {
    import Settings.GitHub

    var user: Option[String] = None; var password: Option[String] = None
    var repositoryOwner: Option[String] = None; var repositoryName: Option[String] = None

    def parseExp(exp: String)(success: Exp[String] => Unit)(error: String => Unit): Unit =
      Selection.Exp.Parser.parse[String](exp) match {
        case Right(exp) => success(exp)
        case Left(msg) => error(msg)
      }

    def parseError(name: String): String => Unit = msg => error(s"Error while parsing `$name`: $msg")
    def parseSelect(name: String, exp: String)(success: Exp[String] => Unit) = parseExp(exp)(success)(parseError(name))

    options foreach {
      case "debug" => Settings.debug = true
      case "warn" => Settings.warn = true
      case GitHub.User(x) => user = Some(x); case GitHub.Password(x) => password = Some(x)
      case GitHub.RepositoryOwner(x) => repositoryOwner = Some(x); case GitHub.RepositoryName(x) => repositoryName = Some(x)
      case Settings.Rules(x) => parseSelect("rules", x) { exp => Settings.rules = exp } 
      case Settings.RuleSet(set, x) => parseSelect(set, x) { exp => Settings.ruleSets = Settings.ruleSets + (set -> exp) }
      case Settings.Warts(x) => parseSelect("warts", x) { exp => Settings.warts = exp }
      case option => error("Option not understood: " + option)
    }

    Settings.github = 
      for { u <- user; p <- password; ro <- repositoryOwner; rn <- repositoryName }
      yield Settings.GitHub(Credentials(u, p), RepositoryId(ro, rn))

    trace(s"""DrScala (warn=${Settings.warn}, github=${Settings.github}, drscala.pr=${GitHub.pullRequestId}""")
    trace(s"Rules: ${Settings.rules}")
    trace(s"RuleSets: ${Settings.ruleSets}")
    trace(s"Warts: ${Settings.warts}")
    trace(s"Doctors: ${doctors.value.map(_.name).mkString(",")}")

    reporterOption = reporterInit
    trace(s"Scope = ${reporterOption.map(_.scope)}")
  }

  override val optionsHelp: Option[String] = Some("""
    |  -P:drscala:
    |      -gh.user=<user>                    
    |      -gh.password=<password>
    |      -gh.repository.owner=<owner>
    |      -gh.repository.name=<name>
    |      -rules=<pattern>                   Select rule sets.
    |      -rules.<ruleset>=<pattern>         Select rules for a given rule set.
    |      -warts=<pattern>                   Select warts (from Wartremover).
    |      -warn                              Generate compiler warnings.
    |      -debug                             Trace plugin debugging information.
  """.trim.stripMargin)
}
