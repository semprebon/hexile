package org.semprebon.hexile

class TileTemplateGenerator(val fileName: String) {
  val HeavyLine = 0.1
  def generateTile(x: Double, y: Double): Unit = {
    val x0 = x + TileTemplateGenerator.Width/2
    val y0 = y + TileTemplateGenerator.Height/2
    drawHex(x0, y0, 4.0, HeavyLine)
  }

  def drawHex(x0: Double, y0: Double, scale: Double, thickness: Double): Unit = {
    val polygon = new Polygon(thickness, )
  }
}
/**
  * Created by Andrew on 4/3/2017.
  */
object TileTemplateGenerator extends App {
  val FileName = if (args.length > 0) args(0) else "hextemplate.svg"
  val generator = new TileTemplateGenerator(FileName)
  val Width = 4.0
  val Height = Width * 2 * Math.sqrt(3.0) / 3.0

  generator.generateTile(0.0, 0.0)
  generator.generateTile(Width, 0.0)
  generator.generateTile(0.0, Height)
  generator.generateTile(Width, Height)
}

