package org.semprebon.hexile

import org.scalatest._
import Matchers._

import scala.xml.{Node, XML}

class PolygonTest extends FlatSpec with Matchers {

  val Triangle = new Polygon("A Triangle", (0.0, 0.0), (0.0, 1.0), (1.0, 0.0))
  val Square = new Polygon("A Square", (0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0))

  "A polygon" should "have correct verticies" in {
    Triangle.vertexes.size shouldEqual(3)
  }

  "A Polygon" should "merge with an adjacent polygon" in {
    val offsetSquare = Square.translate(0.0, -1.0)
    val merged = Triangle.merge(offsetSquare)

    merged.vertexes should equal(
      Triangle.vertexes ++ List(3,0).map(i => offsetSquare.vertexes(i)))
  }

  "A Polygon" should "not merge with a non-adjacent polygon" in {
    val offsetSquare = Square.translate(0.0, -2.0)
    val merged = Triangle.merge(offsetSquare)

    merged.vertexes should equal(Nil)
  }

  "A Polygon" should "translate to a new position" in {
    val shifted = Triangle.translate(1.0, 2.0)
    shifted.vertexes shouldEqual(List((1.0, 2.0), (1.0,3.0), (2.0,2.0)))
  }

  "A polygon" should "generate SVG" in {
    println(Triangle.toSVG(state => "green").toString)
    val xml = Triangle.toSVG(state => "green")

    xml.label should equal("polygon")
    (xml \\ "@fill").toString should equal("green")
    (xml \\ "@points").toString().split(" ").size should equal(3)
  }

  "Polygon" should "merge polygons" in {
    val offsetSquare = Square.translate(0.0, -1.0)
    val merged = Polygon.merge(Triangle, offsetSquare)
    merged.vertexes should equal(
      Triangle.vertexes ++ List(3,0).map(i => offsetSquare.vertexes(i)))
  }
}
