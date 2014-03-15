package drscala

import scala.concurrent.{ExecutionContext, Future}

import github._
import Client._

// TODO Refactor this ugly animal
// TODO Improve laziness by having Reporter.apply to return a Reporter, and have the Future in Reporter (Future[Scope])

class Reporter(client: Client, pr: PR, val scope: Scope) extends PartialFunction[FileName, Seq[Reporter.Report] => Future[Unit]] {
  import scala.concurrent.ExecutionContext.Implicits.global

  final val files: Seq[FileName] = scope.flatMap(_._2.map(_._1))

  // TODO Optimisation: load only needed filename (lazy)
  lazy val writerByCommit = Future.sequence(scope.map { case (commit, _) => 
    // TODO Validate commits ordering
    client.comments(commit).map { comments =>
      val write: ((FileName, CommentAdd)) => Unit = _ match {
        case (filename, comment) =>
          import comment._
          if (!comments.exists { c => c.line == line && c.body == body }) {
            client.comment(pr, comment).onFailure { case t: Throwable => t.printStackTrace }
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
        writerByCommit(commit)(filename -> CommentAdd(commit.id, body, filename, line))
      }
    }
  }

  def isDefinedAt(x: FileName): Boolean = files.contains(x)
}

object Reporter {
  case class Report(body: String, column: Column, line: Line)

  def apply(credentials: Credentials, repositoryId: RepositoryId, pullRequestId: Int)(implicit ec: ExecutionContext): Future[Reporter] = {
    val client = new Client(credentials)
    val pr = new Client.PR(repositoryId, pullRequestId)
    client.scope(pr).map(new Reporter(client, pr, _))
  }
}

