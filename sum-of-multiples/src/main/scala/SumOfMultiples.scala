import scala.annotation.tailrec

object SumOfMultiples {
  def getMultiplesOfFactor(factor: Int, limit: Int): Set[Int] = {
    val multiples = for (i <- 1 to (limit-1)/factor) yield i * factor
    multiples.toSet
  }

  def getFactorsMultiples(factors: Set[Int], limit: Int): Set[Int] = {
    @tailrec
    def iter(factors: Set[Int], limit: Int, accum: Set[Int]): Set[Int] = {
      if (factors.size > 0)
        iter(factors.tail, limit, accum ++ getMultiplesOfFactor(factors.head, limit))
      else accum
    }
    iter(factors, limit, Set(0))
  }

  def sum(factors: Set[Int], limit: Int): Int = {
    getFactorsMultiples(factors, limit).reduce(_ + _)
  }
}

