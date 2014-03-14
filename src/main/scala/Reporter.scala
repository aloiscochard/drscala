package drscala

import scala.concurrent.Future
import scala.util.control.Exception._

import akka.actor.ActorSystem

import github._
import Client._

// TODO Refactor this ugly animal

class Reporter(client: Client, pr: PR, val scope: Scope) extends PartialFunction[FileName, Seq[Reporter.Report] => Future[Unit]] {
  import scala.concurrent.ExecutionContext.Implicits.global

  final val files: Seq[FileName] = scope.flatMap(_._2.map(_._1))

  // TODO Optimisation: load only needed filename (lazy)
  lazy val writerByCommit = Future.sequence(scope.map { case (commit, _) => 
    // TODO Validate commits ordering
    client.comments(commit).map { comments =>
      val write: ((FileName, Comment)) => Unit = _ match {
        case (filename, comment) =>
          import comment._
          if (!comments.exists { c => comment.line == line && comment.body == body }) {
            client.comment(pr, Comment(UserRef(client.credentials.user), filename, body, line, line))
          }
      }
      (commit, write)
    }
  }).map(_.toMap)

  def apply(filename: FileName) = reports => writerByCommit.map { writerByCommit =>
    reports.foreach { report => scope.view.flatMap { 
      case (commit, xs) => 
        import report._
        xs.collectFirst { 
          case (_filename, (start, end)) if (filename.endsWith(_filename)) && line >= start && line <= end => (_filename, commit)
        }
      }.headOption.foreach { case (filename, commit) =>
        import report._
        writerByCommit(commit)(filename -> Comment(UserRef(client.credentials.user), filename, body, line, line))
      }
    }
  }

  def isDefinedAt(x: FileName): Boolean = files.contains(x)
}

object Reporter {
  case class Report(body: String, column: Column, line: Line)

  def apply(credentials: Credentials, repositoryId: RepositoryId, pullRequestId: Int)(implicit as: ActorSystem): Either[Throwable, Reporter] = allCatch.either {
    val client = new Client(credentials)
    val pr = new Client.PR(repositoryId, pullRequestId)

    import scala.concurrent.Await
    import scala.concurrent.duration._

    new Reporter(client, pr, Await.result(client.scope(pr), Duration.Inf))
  }
}

