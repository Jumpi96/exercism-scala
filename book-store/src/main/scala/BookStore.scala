import scala.collection.mutable.HashMap

object BookStore {

  private val price: Double = 800

  private val discounts = Map[Int, Double](
    1 -> 1,
    2 -> 0.95,
    3 -> 0.9,
    4 -> 0.8,
    5 -> 0.75
  )

  private var cache = new HashMap[List[Int], Double]()

  def total(books: List[Int]): Double = {
    cache.getOrElseUpdate(books, doTotal(books))
  }

  def doTotal(books: List[Int]): Double = books match {
    case List() => 0
    case _ => {
      calculateCombinations(books.distinct)
        .map((x: List[Int]) => 
          x.size * price * discounts(x.size) + total(books.diff(x))
        )
        .min
    }
  }

  def calculateCombinations(books: List[Int]): List[List[Int]] = {
    val selection = (1 to books.size)
    selection.flatMap(books.combinations).toList
  }
}
