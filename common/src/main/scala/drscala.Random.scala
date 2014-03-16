package drscala

object Random {
  val random = new scala.util.Random
  def ofTwo[T](one: T, two: T): T = if (random.nextBoolean) one else two
}
