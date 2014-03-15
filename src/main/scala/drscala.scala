package object drscala {
  implicit class RichAny[A](x: A) {
    def const[B]: B => A = _ => x
  }

  class Prefix(value: String) { def unapply(xs: String): Option[String] = if (xs.startsWith(value)) Some(xs.drop(value.size)) else None }
}
