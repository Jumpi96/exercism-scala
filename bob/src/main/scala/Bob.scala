import scala.util.matching.Regex

object Bob {
  
  private val shouting = "([A-Z, 0-9%^*@#$(!]*!?)".r
  private val yelling_asking = "([A-Z 0-9]*\\?)".r
  private val number_asking = "([0-9, ]*\\?)".r
  private val asking = "(.*\\?)".r
  private val nothing = "([ 	]*)".r
  private val numbers = "([0-9, ]*)".r

  def response(input: String): String = {
    val statement = input.trim()
    statement match {
      case nothing(statement) => "Fine. Be that way!"
      case numbers(statement) => "Whatever."
      case number_asking(statement) => "Sure."
      case yelling_asking(statement) => "Calm down, I know what I'm doing!"
      case asking(statement) => "Sure."
      case shouting(statement) => "Whoa, chill out!"
      case _ => "Whatever."
    }
  }

}
