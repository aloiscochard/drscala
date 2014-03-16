package drscala

object Selection {

  sealed abstract class Exp[A] extends (Seq[A] => Seq[A]) {
    import Exp._, Select._

    def map[B](f: A => B): Exp[B] = this match {
      case All() => All()
      case None() => None()
      case Select(mode, diff) => Select[B](mode, diff.map(f))
    }

    def nonEmpty: Option[Exp[A]] = this match {
      case None() | Select(Inclusive, Nil) => scala.None
      case exp => Some(exp)
    }

    override def toString = this match {
      case All() => "All"
      case None() => "None"
      case Select(mode, diff) => mode match {
        case Inclusive => s"Include($diff)"
        case Exclusive => s"Exclude($diff)"
      }
    }
  }

  object Exp {

    def exclude[A](xs: Seq[A]) = Select(Select.Exclusive, xs)
    def include[A](xs: Seq[A]) = Select(Select.Inclusive, xs)

    case class All[A]() extends Exp[A] { override def apply(xs: Seq[A]) = xs }
    case class None[A]() extends Exp[A] { override def apply(xs: Seq[A]) = xs }

    case class Select[A](val mode: Select.Mode, diff: Seq[A]) extends Exp[A] {
      override def apply(xs: Seq[A]) = mode match {
        case Select.Exclusive => (xs.toSet -- diff.toSet).toSeq
        case Select.Inclusive => (diff.toSet -- (diff.diff(xs)).toSet).toSeq
      }
    }

    object Select { 
      sealed abstract class Mode(val symbol: String)
      case object Inclusive extends Mode("+")
      case object Exclusive extends Mode("-")
    }

    import scala.util.parsing.combinator._
    import scala.util.matching._

    object Parser {

      /* all  = All
       * none = None
       * +1+2 = Select(None, [1, 2])
       * -1-2 = Select(All, [1, 2])
       */
      def parse[A : Format](input: String): Either[String, Exp[A]] = (new Parser).parse(input)

      case class Format[A](regex: Regex)(val f: String => A)

      object Format {
        implicit val string = new Format[String]("""'[^']*'"""r)(x => x.substring(1, x.length - 1))
        implicit val int = new Format[Int]("""\d*"""r)(_.toInt)
      }
    }

    class Parser[A](implicit format: Exp.Parser.Format[A]) extends RegexParsers {
      import Select._

      def parse(input: String) = parseAll(exp, input) match {
        case Success(exp, _) => Right(exp)
        case err: NoSuccess => Left(s"${err.next.offset}: ${err.msg}")
      }

      val value: Parser[A] = format.regex.map(format.f)

      val all: Parser[All[A]] = matchCaseInsensitive("all").map(All().const)

      val none: Parser[None[A]] = matchCaseInsensitive("none").map(None().const)

      val mode: Parser[Mode] = Seq(Inclusive, Exclusive).map { mode =>
        matchCaseInsensitive(mode.symbol).map(mode.const)
      }.reduce(_ | _)

      val select: Parser[Select[A]] = 
        mode.flatMap(mode => (value ~ (mode.symbol ~> value).*).map { case x ~ xs => Select(mode, x :: xs) })

      val exp: Parser[Exp[A]] = (all | none | select)

      private def matchCaseInsensitive(xs: String*) = 
        xs.toList.flatMap(x => x :: x.toUpperCase :: Nil).map(Parser(_)).reduceLeft(_ | _).map(_.toLowerCase)
    }
  }
}

