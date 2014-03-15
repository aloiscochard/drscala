package drscala

import scala.concurrent.ExecutionContext
import dispatch._, Defaults._
import argonaut._, Argonaut._

package object github {
  type Body = String
  type Column = Int
  type FileName = String
  type Line = Int
  type Range = (Int, Int)
  // TODO Lazy loaded scope = Seq[(Client.Commit, Future[Seq[(FileName, Range)]])] 
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
  object RepositoryId { def apply(owner: String, name: String) = s"https://api.github.com/repos/$owner/$name" }
}

package github {
  object Client {

    object Codecs {
      implicit val f = casecodec3(FileRef.apply, FileRef.unapply)("filename", "patch", "raw_url")
      implicit val u = casecodec1(UserRef.apply, UserRef.unapply)("login")
      implicit val c = casecodec4(Comment.apply, Comment.unapply)("user", "path", "body", "position")
      implicit val ca = casecodec4(CommentAdd.apply, CommentAdd.unapply)("commit_id", "body", "path", "position")
      implicit val co = casecodec3(Commit.apply, Commit.unapply)("sha", "files", "comments_url")
      implicit val cr = casecodec2(CommitRef.apply, CommitRef.unapply)("url", "comments_url")
    }

    class PR(val repo: RepositoryId, prId: Int) {
      case class Paths(repo: RepositoryId, prId: Int) {
        val root = s"$repo/pulls/$prId"
        def commits: String = s"$root/commits"
        def comments: String = s"$root/comments"
      }

      val paths = Paths(repo, prId)
    }

    case class Commit(
      id: String,
      files: List[FileRef],
      commentsUrl: String
    ) extends HasComments

    case class CommitRef(
      url: String,
      commentsUrl: String
    ) extends HasComments

    trait HasComments {
      def commentsUrl: String
    }

    case class FileRef(
      fileName: FileName,
      patch: String,
      rawUrl: String
    ) { override def toString = s"FileRef($fileName)" }

    case class Comment(
      user: UserRef, 
      path: FileName,
      body: Body, 
      line: Line
    )

    case class CommentAdd(
      commitId: String,
      body: String,
      path: FileName,
      line: Line
    )
    
    case class UserRef(login: String)
  }

  class Client(val credentials: Credentials)(implicit ec: ExecutionContext) {
    import Client._
    import Codecs._

    protected def get[T](path: String)(implicit d: DecodeJson[T]): Future[Option[T]] =
      Http(url(path).as_!(credentials.user, credentials.password) OK as.String).map(_.decodeOption[T])

    def scope(pr: PR): Future[Scope] = for {
      xs <- commits(pr)
      ys <- Future.sequence(xs.map(commit))
    } yield ys.flatMap(_.map { commit =>
      commit -> commit.files.flatMap(f => Range(f.patch).map(r => f.fileName -> r))
    })

    def commits(pr: PR): Future[Seq[CommitRef]] = get[List[CommitRef]](pr.paths.commits).map(_.getOrElse(Nil))
    def commit(commitRef: CommitRef): Future[Option[Commit]] = get[Commit](commitRef.url)
    def comments(ref: HasComments): Future[Seq[Comment]] = get[List[Comment]](ref.commentsUrl).map(_.getOrElse(Nil))

    def comment(pr: PR, comment: CommentAdd): Future[Option[Comment]] = Http(
      (url(s"${pr.repo}/commits/${comment.commitId}/comments").as_!(credentials.user, credentials.password).POST << comment.asJson.nospaces) OK as.String)
      .map(_.decodeOption[Comment]
    )
  }
}
