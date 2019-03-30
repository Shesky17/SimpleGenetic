package tests

import org.scalatest._
import genetics.GeneticAlgorithm._
import genetics.geometry._

class TestLinearRegression extends FunSuite
{
  val EPSILON = 0.1
  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test(""){
    //x+1
    val ptList1 = List(new Point(5,6), new Point(6,7), new Point(7,8))
    //3x+2
    val ptList2 = List(new Point(1,5), new Point(4,14), new Point(10,32), new Point(9,29))
    //6x+1
    val ptList3 = List(new Point(1,7), new Point(6,37), new Point(7,43))
    //-x+3
    val ptList4 = List(new Point(1,2), new Point(6,-3), new Point(7,-4))
    //y=0.533x
    val ptList5 = List(new Point(1,0.533), new Point(6, 3.198), new Point(7,3.731))

    val slope1 = 1.0
    val y1 = 1.0
    val shit1 = linearRegression(ptList1)

    val slope2 = 3.0
    val y2 = 2.0
    val shit2 = linearRegression(ptList2)

    val slope3 = 6.0
    val y3 = 1.0
    val shit3 = linearRegression(ptList3)

    val slope4 = -1.0
    val y4 = 3.0
    val shit4 = linearRegression(ptList4)

    val slope5 = 0.533
    val y5 = 0.0
    val shit5 = linearRegression(ptList5)

    //println(linearRegression(ptList5))

    assert(equalDoubles(shit1.slope, slope1) && equalDoubles(shit1.yIntercept, y1))
    assert(equalDoubles(shit2.slope, slope2) && equalDoubles(shit2.yIntercept, y2))
    assert(equalDoubles(shit3.slope, slope3) && equalDoubles(shit3.yIntercept, y3))
    assert(equalDoubles(shit4.slope, slope4) && equalDoubles(shit4.yIntercept, y4))
    assert(equalDoubles(shit5.slope, slope5) && equalDoubles(shit5.yIntercept, y5))
  }
}
