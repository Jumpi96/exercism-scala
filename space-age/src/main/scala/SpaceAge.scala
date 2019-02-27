
object SpaceAge {

  private val onEarthYears = Map[Symbol, Double](
    'earth -> 1,
    'mercury -> 0.2408467,
    'venus -> 0.61519726,
    'mars -> 1.8808158,
    'jupiter -> 11.862615,
    'saturn -> 29.447498,
    'uranus -> 84.016846,
    'neptune -> 164.79132
  )

  private val earthYearOnSeconds = 31557600

  def onEarth(seconds: Double): Double = getYears('earth, seconds)
  def onMercury(seconds: Double): Double = getYears('mercury, seconds)
  def onVenus(seconds: Double): Double = getYears('venus, seconds)
  def onMars(seconds: Double): Double = getYears('mars, seconds)
  def onJupiter(seconds: Double): Double = getYears('jupiter, seconds)
  def onSaturn(seconds: Double): Double = getYears('saturn, seconds)
  def onUranus(seconds: Double): Double = getYears('uranus, seconds)
  def onNeptune(seconds: Double): Double = getYears('neptune, seconds)

  def getYears(planet: Symbol, seconds: Double): Double = {
    (seconds / earthYearOnSeconds) / onEarthYears(planet)
  }
  
}
