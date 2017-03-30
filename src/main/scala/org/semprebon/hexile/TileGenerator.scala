package org.semprebon.hexile

/**
  * Creates an SVG file from inputs
  *
  * * SvgGenerator <edge-fills>...
  *
  * Example: To create a hextile SVG for a gray road path N/S path through green grass:
  *
  * SvgGenerator gray green green grey green green
  */
object TileGenerator extends App {

  val tile = new HexTile(args:_*)
  print(tile.toSVG())
}
