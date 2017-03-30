package org.semprebon.hexile

import org.scalatest._
import scala.xml._

class HexTileTest extends FlatSpec with Matchers {

  val Open = 0
  val Wall = 1

  val openHexTile = new HexTile[Int](Open)
  val pathHexTile = new HexTile(Open, Wall, Wall, Open, Wall, Wall)

  "An open hextile" should "have 1 hexigon" in {
    openHexTile.polygons.size should equal(1)
    openHexTile.polygons(0).edges.size should equal(6)
  }

  "An open hextile" should "generate SVG" in {
    val xml = openHexTile.toSVG()
    xml.label should equal("svg")
    (xml \\ "polygon" \\ "@points").toString().split(" ").size should equal(6)
  }

  "A path hextile" should "have 3 polygons" in {
    pathHexTile.polygons.size should equal(3)

    val topEdgeCoords = HexTile.UnitHexEdgeCoordinates.take(2)
    val path = pathHexTile.polygons
      .filter { p => p.vertexes.contains(topEdgeCoords(0)) && p.vertexes.contains(topEdgeCoords(1)) }
      .head
    path.vertexes.size should equal(4)
    path.state should equal(Open)

    val left = pathHexTile.polygons
      .filter { p => p.vertexes.contains(HexTile.UnitHexEdgeCoordinates(5)) }
      .head
    left.vertexes.size should equal(3)
    left.state should equal(Wall)

    val right = pathHexTile.polygons
      .filter { p => p.vertexes.contains(HexTile.UnitHexEdgeCoordinates(2)) }
      .head
    right.vertexes.size should equal(3)
    right.state should equal(Wall)
  }
}
