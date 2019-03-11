import scala.annotation.tailrec

class Accumulate {
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = {
    @tailrec
    def iter(f: (A) => B, list : List[A], accum : List[B]): List[B] = list match {
      case head :: rest => iter(f, rest, f(head) :: accum)
      case Nil => accum.reverse
    }
    iter(f, list, List[B]())
  }
}
