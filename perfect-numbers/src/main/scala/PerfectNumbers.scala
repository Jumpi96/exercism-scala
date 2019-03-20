
object NumberType extends Enumeration {
  type NumberType = Value
  val Perfect, Abundant, Deficient = Value
}

object PerfectNumbers {

  def classify(number: Int): Either[String, Object] = number match {
    case 0 => Left("Classification is only possible for natural numbers.")
    case _ if number < 0 => Left("Classification is only possible for natural numbers.")
    case _ => Right(typeOfNumber(number))
  }

  def typeOfNumber(number: Int) = {
    val sum: Int = aliquotSum(number)
    if (sum == number)
      NumberType.Perfect
    else if (sum > number)
      NumberType.Abundant
    else
      NumberType.Deficient
  }

  def aliquotSum(number: Int): Int = {
    val divisors = for (i <- 1 to number / 2) yield i
    divisors.filter(number % _ == 0).sum
  }

}