package object drscala {
  implicit class RichAny[A](x: A) { def const[B]: B => A = _ => x }

  type Column = Int
  type Line = Int
  type Position = (Line, Column)
  type Message = String
  type Universe = reflect.api.Universe

  class Prefix(value: String) { def unapply(xs: String): Option[String] = if (xs.startsWith(value)) Some(xs.drop(value.size)) else None }
  class Suffix(value: String) { def unapply(xs: String): Option[String] = if (xs.endsWith(value)) Some(xs.take(xs.size - value.size)) else None }

  trait Lazy[A] { self =>
    protected def get: A
    def value: A = get
  }

  object Lazy { 
    def apply[A](x: => A) = new Lazy[A] {
      def get = x
      lazy override val value: A = get 
    }
  }

  object PackageObjects {
    import scala.language.experimental.macros
    import scala.reflect.macros.Context

    def apply[T](packageName: String) = macro apply_impl[T]

    def apply_impl[T: c.WeakTypeTag](c: Context)(
      packageName: c.Expr[String]
    ) = {
      import c.universe._

      val tpe = weakTypeOf[T]
      val pkg = packageName.tree match {
        case Literal(Constant(name: String)) => c.mirror.staticPackage(name)
      }

      val objs = pkg.typeSignature.members.collect {
        case sym: ModuleSymbol if sym.typeSignature <:< tpe => sym
      }.toList

      c.Expr[Set[T]] {
        Apply(
          Select(
            Select(Select(Select(Ident(newTermName("scala")), newTermName("collection")), newTermName("immutable")), newTermName("Set")),
            newTermName("apply")
          ), 
          objs.map(Ident(_)).to[List]
        )
      }
    }
  }
}
