package org.semprebon.hexile

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Represents a polygon with associated state
  */
class Polygon[T](val state: T, val vertexes: Tuple2[Double, Double]*) {
  // TODO: Figure out why putting these in a package object doesn't work
  type Point = Tuple2[Double, Double]
  type Edge = Tuple2[Point, Point]

  def translate(offset: Point): Polygon[T] = {
    new Polygon[T](state, vertexes.map(p => (p._1 + offset._1, p._2 + offset._2)):_*)
  }

  def rotate(n: Int, list: Seq[Edge]) = list.drop(n) ++ list.take(n)

  def edges: Seq[Edge] = vertexes.zip(vertexes.drop(1) :+ vertexes(0))

  def findAdjoiningEdge(edge: Edge, Edges: Seq[Edge]): Int = edges.indexOf(edge.swap)

  def isAdjoining(polygon: Polygon[T]): Boolean = {
    return edges.exists(edge => findAdjoiningEdge(edge, polygon.edges) == -1)
  }

  /**
    * Merges two polygons on their adjoining edges. Both must be oriented in
    * the same direction and are non overlapping.
    *
    * Basic algorythm recursively moves points from A or B to the result, until
    * both are empty, removing adjoining edges as it goes, and ensuring edges are
    * preserved.
    */
  def merge(poly: Polygon[T]): Polygon[T] = {

    def areEdgeConnected(a: Edge, b: Edge): Boolean = a._2 == b._1

    def isConnected(a: Seq[Edge], b: Seq[Edge]): Boolean = areEdgeConnected(a.last, b(0))

    def connectOrDie(a: Seq[Edge],  b: Seq[Edge]) = {
      val connectingIndex = b.indexWhere(edge => areEdgeConnected(a.last, edge))
      if (connectingIndex == -1) {
        Nil
      } else {
        val x = rotate(connectingIndex, b)
        a ++ rotate(connectingIndex, b)
      }
    }

    def addEdge(result: Seq[Edge], a: Seq[Edge], b: Seq[Edge]): Seq[Edge] = {
      if (a.isEmpty && b.isEmpty) return result
      else if (a.isEmpty) connectOrDie(result, b)
      else if (b.isEmpty) connectOrDie(result, a)
      else {
        val adjoiningIndex = b.indexOf(a(0).swap)
        if (adjoiningIndex == -1) {
          addEdge(result :+ a(0), a.drop(1), b)
        } else {
          val x = rotate(adjoiningIndex, b).drop(1)
          addEdge(result, a.drop(1), rotate(adjoiningIndex, b).drop(1))
        }
      }
    }

    val resultEdges = addEdge(List[Edge](), edges, poly.edges)

    new Polygon[T](state, resultEdges.map(edge => edge._1):_*)
  }

  def toSVG(stateToFill: T => String) = {
    //<polygon fill={ stateToFill(state) }
    <polygon fill={ stateToFill(state) }
      points={ vertexes.map((v) => v._1 + "," + v._2).reduce(_ + " " + _) }></polygon>
  }
}

object Polygon {

  def merge[T](polygons: Seq[Polygon[T]]): Seq[Polygon[T]] = {
    if (polygons.size == 0) return polygons
    val result = ListBuffer[Polygon[T]]()
    var available = ArrayBuffer[Polygon[T]]() ++= polygons

    while (available.length > 1) {
      val p = available.remove(0)
      val i = available.indexWhere(_.isAdjoining(p))

      if (i == -1) result += p
      else available += p.merge(available.remove(i))
    }
    result ++= available
    return result.toSeq
  }
}
