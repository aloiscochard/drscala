package foo

object Bar {
  def value[T] = 42.asInstanceOf[T]

  def first[T](xs: Seq[T]) = xs.head

  def firstOpt[T](xs: Option[T]) = xs.head

  def run = ???

  println("HelloWorld")
  

  def oget[T](o: Option[T]) = o.get

  def ohno = value

  def `null` = println(null)
}
