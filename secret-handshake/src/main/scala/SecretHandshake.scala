
object SecretHandshake {

  val codes = Map(
    1 -> "wink",
    2 -> "double blink",
    4 -> "close your eyes",
    8 -> "jump"
  )

  def commands(decimal: Int): List[String] = {
    val list = codes.foldLeft(List[String]())((accum, member) =>
      if ((member._1 & decimal) > 0) member._2 :: accum else accum
    )
    if ((math.pow(2, codes.size).toInt & decimal) > 0) list else list.reverse
  }

}