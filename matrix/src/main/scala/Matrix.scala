

class Matrix private (val input: String) {

  private val matrix: Vector[Vector[Int]] = processString(input)

  def row(m: Int): Vector[Int] = matrix(m)

  def column(n: Int): Vector[Int] = matrix.map(_(n))

  def processString(input: String): Vector[Vector[Int]] = {
    input
      .split("\n")
      .toVector
      .map(_.split(" ").map(_.toInt).toVector)
  }

}

object Matrix {
  def apply(input: String) = new Matrix(input)
}