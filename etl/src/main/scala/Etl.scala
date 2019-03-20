object Etl {

  def transform(oldScores: Map[Int, Seq[String]]): Map[String, Int] = {
    oldScores
      .flatMap({
        case (value, letters) => letters.map {
          case letter => letter.toLowerCase -> value
        }
      })
      .toMap
  }

}
