package drscala

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

import GHClient.{Credentials, RepositoryId}

class CompilerPlugin(val global: Global) extends Plugin with HealthCake { import global._

  class Checkup(phase: String, doctors: Seq[Doctor], val global: Global = CompilerPlugin.this.global) extends PluginComponent { import global._
    override val runsAfter = List(phase)
    override val phaseName = s"drscala.${phase}"
    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        def writer = Settings.github.flatMap(_.reporter).flatMap { reporter =>
          reporter.scope.view.flatMap { case (_, xs) => 
            xs.find { case (filename, _) => unit.source.file.path.endsWith(filename) }
              .map { case (filename, _) => reporter(filename) }
          }.headOption
        }

        trace(s"writer@${unit.source.file.path}=$writer")
        if (Settings.warn || writer.isDefined) {
          val comments = doctors.flatMap(_.diagnostic.lift(phase).toSeq.flatMap(_(unit.asInstanceOf[CompilerPlugin.this.global.CompilationUnit])))
          trace(comments.mkString("\n"))
          if (Settings.warn) comments.foreach { case (pos, body) => unit.warning(pos, s"$phaseName\n$body") }
          writer.foreach(_(comments.map { case (pos, body) => (pos.line -> pos.column) -> body }))
        }
      }
    }
  }

  val name = "drscala"
  val description = "A doctor for your code"
  val doctors = Seq(new Doctor.StdLib)
  val components = List("parser", "typer").map(new Checkup(_, doctors))

  object Settings {
    class Prefix(value: String) { def unapply(xs: String): Option[String] = if (xs.startsWith(value)) Some(xs.drop(value.size)) else None }

    case class GitHub(credentials: Credentials, repositoryId: RepositoryId) {
      lazy val pullRequestId = GitHub.pullRequestId
      lazy val reporter = pullRequestId.flatMap(new GHClient(credentials).report(repositoryId, _))
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

    var debug = false
    var warn = false
    var github: Option[GitHub] = None
  }

  private def trace(message: => String): Unit = if (Settings.debug) { println(message) }

  override def processOptions(options: List[String], error: String => Unit) {
    import Settings._

    var user: Option[String] = None; var password: Option[String] = None
    var repositoryOwner: Option[String] = None; var repositoryName: Option[String] = None

    options foreach {
      case "debug" => Settings.debug = true
      case "warn" => Settings.warn = true
      case GitHub.User(x) => user = Some(x); case GitHub.Password(x) => password = Some(x)
      case GitHub.RepositoryOwner(x) => repositoryOwner = Some(x); case GitHub.RepositoryName(x) => repositoryName = Some(x)
      case option => error("Option not understood: " + option)
    }

    Settings.github = 
      for { u <- user; p <- password; ro <- repositoryOwner; rn <- repositoryName }
      yield Settings.GitHub(Credentials(u, p), RepositoryId(ro, rn))

    trace(s"""DrScala (warn=${Settings.warn}, github=${Settings.github}, drscala.pr=${GitHub.pullRequestId}""")
    trace(doctors.map(_.name).mkString(","))
    trace(s"Scope = ${Settings.github.flatMap(_.reporter).map(_.scope)}")
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
