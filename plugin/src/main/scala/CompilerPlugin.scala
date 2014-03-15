package drscala

import scala.PartialFunction.cond
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception._
import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

import github.{Client, Credentials, RepositoryId}

import doctors._

class CompilerPlugin(val global: Global) extends Plugin with HealthCake 
    with StdComponent with WartComponent { import global.{Lazy => _, _}

  implicit val ec: ExecutionContext = ExecutionContext.global

  trait Checkup extends PluginComponent {
    import global._
    def checkup: CompilerPlugin.this.global.CompilationUnit => Seq[(CompilerPlugin.this.Position, String)]

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

  val doctors = Lazy(Seq(StdLib, StdStyle) ++ Settings.warts.map(WartDoctor.fromString).toSeq)
  val sementics = doctors.map(_.collect { case x: Doctor.Sementic => x})
  val styles = doctors.map(_.collect { case x: Doctor.Style => x})

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

    val Warts = new Prefix("warts=")

    var debug = false

    val disabled = Option(System.getProperty("drscala.disable"))
      .orElse(Option(System.getenv("DRSCALA_DISABLE")))
      .fold(false)(_.toLowerCase == "true")

    var github: Option[GitHub] = None

    var warts: Option[String] = None

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
    import Settings._

    var user: Option[String] = None; var password: Option[String] = None
    var repositoryOwner: Option[String] = None; var repositoryName: Option[String] = None

    options foreach {
      case "debug" => Settings.debug = true
      case "warn" => Settings.warn = true
      case GitHub.User(x) => user = Some(x); case GitHub.Password(x) => password = Some(x)
      case GitHub.RepositoryOwner(x) => repositoryOwner = Some(x); case GitHub.RepositoryName(x) => repositoryName = Some(x)
      case Warts(names) => warts = Some(names)
      case option => error("Option not understood: " + option)
    }

    Settings.github = 
      for { u <- user; p <- password; ro <- repositoryOwner; rn <- repositoryName }
      yield Settings.GitHub(Credentials(u, p), RepositoryId(ro, rn))

    trace(s"""DrScala (warn=${Settings.warn}, github=${Settings.github}, drscala.pr=${GitHub.pullRequestId}""")
    trace(doctors.value.map(_.name).mkString(","))

    reporterOption = reporterInit
    trace(s"Scope = ${reporterOption.map(_.scope)}")
  }

  override val optionsHelp: Option[String] = Some("""
    |  -P:drscala:
    |      -warn                              Generate compiler warnings.
    |      -gh.user=<user>
    |      -gh.password=<password>
    |      -gh.repository.owner=<owner>
    |      -gh.repository.name=<name>
    |      -debug                             Trace plugin debugging information.
  """.trim.stripMargin)
}
