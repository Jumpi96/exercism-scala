import scala.annotation.tailrec

object Hamming {

  def distance(a: String, b: String): Option[Int] = a match {
    case _ if a.length != b.length => None
    case _ => Some(calc_distance(a, b))
  }

  def calc_distance(a: String, b: String): Int = {
    @tailrec
    def iter(a: String, b: String, accum: Int): Int = a match {
      case "" => accum
      case _ => {
        val difference = if (a.head != b.head) 1 else 0
        iter(a.tail, b.tail, difference + accum)
      }
    }
    iter(a, b, 0)
  }
}
