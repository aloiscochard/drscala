package drscala

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

import akka.actor.ActorSystem

import spray.http._
import spray.json.DefaultJsonProtocol
import spray.httpx.encoding.{Gzip, Deflate}
import spray.httpx.SprayJsonSupport._
import spray.client.pipelining._

package object github {
  type Body = String
  type Column = Int
  type FileName = String
  type Line = Int
  type Range = (Int, Int)
  type Scope = Seq[(Client.Commit, Seq[(FileName, Range)])]

  object Range {
    def apply(patch: String): Option[Range] =
      patch.split("\n").headOption.map(_.split("\\+")(1).split("@@")(0).trim).map { targetHunk =>
        val xs = targetHunk.split(",").map(_.toInt)
        (xs(0) -> xs(1))
      }
  }

  case class Credentials(user: String, password: String)

  type RepositoryId = String
  object RepositoryId { def apply(owner: String, name: String) = s"$owner/$name" }
}

package github {
  object Client {

    object JsonProtocol extends DefaultJsonProtocol {
      implicit val f = jsonFormat3(FileRef)
      implicit val u = jsonFormat1(UserRef)
      implicit val c = jsonFormat2(Commit)
      implicit val cm = jsonFormat5(Comment)
      implicit val cr = jsonFormat2(CommitRef)
    }


    class PR(repo: RepositoryId, prId: Int) {
      case class Paths(repo: RepositoryId, prId: Int) {
        val root = "https://api.github.com/repos/$repo/pulls/$prId"
        def commits: String = s"$root/commits"
        def comments: String = s"$root/comments"
      }

      val paths = Paths(repo, prId)
    }

    case class Commit(
      `comments_url`: String,
      files: Seq[FileRef]
    ) extends HasComments

    case class CommitRef(
      url: String,
      `comments_url`: String
    ) extends HasComments

    trait HasComments {
      def `comments_url`: String
    }

    case class FileRef(
      filename: FileName,
      patch: String,
      raw_url: String
    )

    case class Comment(
      user: UserRef, 
      path: FileName,
      body: Body, 
      position: Column,
      line: Line
    )
    
    case class UserRef(login: String)
  }

  class Client(val credentials: Credentials)(implicit as: ActorSystem) {
    import as.dispatcher
    import Client._
    import JsonProtocol._

    val httpCredentials = BasicHttpCredentials(credentials.user, credentials.password)
    val pipeline = addCredentials(httpCredentials) ~> sendReceive 

    def commits(pr: PR): Future[Seq[CommitRef]] = 
      (pipeline ~> unmarshal[Seq[CommitRef]]).apply(Get(pr.paths.commits))

    def comment(pr: PR, comment: Comment): Future[Comment] = 
      (pipeline ~> unmarshal[Comment]).apply(Post(pr.paths.comments, comment))

    def scope(pr: PR): Future[Scope] = for {
      xs <- commits(pr)
      ys <- Future.sequence(xs.map(commit))
    } yield ys.map { commit =>
      commit -> commit.files.flatMap(f => Range(f.patch).map(r => f.filename -> r))
    }

    def commit(commitRef: CommitRef): Future[Commit] =
      (pipeline ~> unmarshal[Commit]).apply(Get(commitRef.url))

    def comments(ref: HasComments): Future[Seq[Comment]] =
      (pipeline ~> unmarshal[Seq[Comment]]).apply(Get(ref.`comments_url`))
  }
}
