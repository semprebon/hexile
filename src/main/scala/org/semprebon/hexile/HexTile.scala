package org.semprebon.hexile

import scala.math.{Pi, cos, sin}

/**
  * Hextile represents a hex tile, currently, a single hex
  *
  *
  * Labelling Points and Edges
  *             0 1             0
  * Vertices:  5 * 2   Edges: 5   1
  *             4 3           4   2
  *                             3
  */
class HexTile[T](val edges: T*) {

  if (edges.size != 6) throw new IllegalArgumentException("Must specify 6 edge values")

  def this(state: T) { this(HexTile.EdgeRange.map((i) => state): _*) }

  def toSVG() =
    <svg version="1.1" xmlns="http://www.w3.org/2000/svg">
      <polygon points={ HexTile.Vertexes.map((v) => v.toString()).reduce(_ + " " + _) }></polygon>
    </svg>

  def polygons: Seq[Polygon[T]] = {
    val edgeIndexesAndTypeGroupedByState: Map[T, Seq[(T, Int)]] =
      edges.zipWithIndex
        .groupBy(_._1)

    val edgeIndexesGroupedByState: Map[T, Seq[Int]] =
      edgeIndexesAndTypeGroupedByState.map(p => (p._1, p._2.map(_._2)))

    val polygons: Seq[Polygon[T]] = edgeIndexesGroupedByState.toSeq
      .flatMap { stateAndEdges =>
        val (state, edges) = stateAndEdges
        val edgePolygons = edges.map(edge => HexTile.polygonForEdge(edge, state))
        Polygon.merge(edgePolygons)
      }
    return polygons
  }

}

object HexTile {
  // TODO: Figure out why putting these in a package object doesn't work
  type Point = Tuple2[Double, Double]
  type Edge = Tuple2[Point, Point]

  val EdgeRange = (0 to 5)

  val N = 0
  val NE = 1
  val SE = 2
  val S = 3
  val SW = 4
  val NW = 5

  val Vertexes: List[Point] = HexTile.EdgeRange.toList
    .map((i) => (i + 4) * Pi / 3.0)
    .map( (a) => new Point(cos(a), -sin(a)) )

  val UnitHexEdgeCoordinates = EdgeRange.map(i => List(Vertexes(i), Vertexes(i % 6)))

  val UnitHexHeight = Math.sqrt(3.0) / 2.0

  val InternalVertexes = List(-1, 1).map(x => new Point(x*UnitHexHeight/4, 0.0))

  // This contains the polygon associated with each edge
  val EdgePolygons = List[List[Point]](
    UnitHexEdgeCoordinates(N)  ++ List(InternalVertexes(1), InternalVertexes(0)),
    UnitHexEdgeCoordinates(NE) ++ List(InternalVertexes(1)),
    UnitHexEdgeCoordinates(SE) ++ List(InternalVertexes(1)),
    UnitHexEdgeCoordinates(S)  ++ List(InternalVertexes(0), InternalVertexes(1)),
    UnitHexEdgeCoordinates(SW) ++ List(InternalVertexes(0)),
    UnitHexEdgeCoordinates(NW) ++ List(InternalVertexes(0)))

  def polygonForEdge[T](edgeIndex: Int, edgeState: T): Polygon[T] = {
    new Polygon[T] (edgeState, EdgePolygons(edgeIndex):_*)
  }
}
