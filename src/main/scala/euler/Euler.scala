package euler

object Euler extends App {
  implicit class FactorialInt(val num: Int) extends AnyVal {
    def ! : BigInt = (BigInt(1) to num).product
  }

  def factorial(num: Int): BigInt = (BigInt(1) to num).product

  println(factorial(20))
  println(20!)
  println((40!)/((20!) * (20!)))
}
