package genetics.geometry

class Line(val slope: Double, val yIntercept: Double) {

  def evaluate(x: Double): Double = {
    slope * x + yIntercept
  }

  override def toString: String = {
    f"y = $slope%1.3fx + $yIntercept%1.3f"
  }

}
