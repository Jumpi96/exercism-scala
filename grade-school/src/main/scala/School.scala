class School {
  type DB = Map[Int, Seq[String]]

  private var dbMap = Map[Int, Seq[String]]()

  def add(name: String, g: Int) = {
    dbMap = dbMap + (g -> (grade(g) :+ name))
  }

  def db: DB = dbMap

  def grade(g: Int): Seq[String] = dbMap.getOrElse(g, Seq[String]())

  def sorted: DB = dbMap.mapValues(_.sorted).toSeq.sortBy(_._1).toMap
}
