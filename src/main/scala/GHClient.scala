package drscala

import scala.concurrent.Future
import scala.collection.JavaConverters._
import scala.util.control.Exception._

import org.kohsuke.github.GitHub
import org.kohsuke.github.GHRepository

class GHClient(credentials: GHClient.Credentials) { import GHClient._
  import GHClient._

  private val client = GitHub.connectUsingPassword(credentials.user, credentials.password)

  def report(repositoryId: RepositoryId, pullRequestId: Int): Option[Reporter] =
    for {
      repository <- allCatch.opt(client.getRepository(repositoryId))
      pullRequest <- allCatch.opt(repository.getPullRequest(pullRequestId))
      compare <- allCatch.opt(repository.getCompare(pullRequest.getBase.getSha, pullRequest.getHead.getSha))
    } yield {
      val scope = compare.getCommits.map(commit => repository.getCommit(commit.getSHA1)).flatMap { commit =>
        commit.getFiles.asScala.flatMap { file =>
          file.getPatch.split("\n").headOption.map(_.split("\\+")(1).split("@@")(0).trim).map { targetHunk =>
            val xs = targetHunk.split(",").map(_.toInt)
            file.getFileName -> (xs(0) -> xs(1))
          }
        }.toSeq match {
          case Nil => None
          case xs => Some(commit.getSHA1 -> xs)
        }
      }
      new Reporter(repository, scope)
    }
}

object GHClient {
  type Body = String
  type Column = Int
  type CommitSha = String
  type Comment = (Position, Body)
  type FileName = String
  type Line = Int
  type Position = (Line, Column)
  type Range = (Int, Int)
  type Scope = Seq[(CommitSha, Seq[(FileName, Range)])]

  case class Credentials(user: String, password: String)

  type RepositoryId = String
  object RepositoryId { def apply(owner: String, name: String) = s"$owner/$name" }

  class Reporter private[GHClient](repository: GHRepository, val scope: Scope) extends PartialFunction[FileName, Seq[Comment] => Future[Unit]] {
    import scala.concurrent.ExecutionContext.Implicits.global

    final val files: Seq[FileName] = scope.flatMap(_._2.map(_._1))

    // TODO Optimisation: load only needed filename (lazy)
    lazy val writerByCommit = scope.map { case (sha, _) => 
      // TODO Validate commits ordering
      val commit = repository.getCommit(sha)
      val comments = commit.listComments.asList.asScala.map(comment => (comment.getLine -> -1, comment.getBody))
      val write: ((FileName, Comment)) => Unit = _ match {
        case (filename, ((line, column), body)) =>
          if (!comments.exists { case ((l,_), b) => l == line && b == body }) {
            commit.createComment(body, filename, line, line)
          }
      }
      (sha, write)
    }.toMap

    def apply(filename: FileName) = comments => Future {
      comments.foreach { 
        case comment@((line, column), body) => scope.collectFirst { 
          case (sha, xs) => xs.collectFirst { 
            case (_filename, (start, end)) if (_filename.endsWith(filename)) && line >= start && line <= end => (_filename, sha)
          }
        }.flatten.foreach { case (filename, sha) =>
          writerByCommit(sha)(filename -> comment)
        }
      }
    }

    def isDefinedAt(x: FileName): Boolean = files.contains(x)
  }

}

