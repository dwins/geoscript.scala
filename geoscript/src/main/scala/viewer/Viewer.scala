package org.geoscript

package object viewer {
  def showWindow() {
    val window = new javax.swing.JFrame
    window.setDefaultCloseOperation(
      javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
    window.setSize(512, 512)
    window.setVisible(true)
    window.add(geometryComponent)
  }

  private var visibleGeometries = Seq.empty[geometry.Geometry]

  def worldToScreen(
    rect: java.awt.Rectangle,
    env: geometry.Envelope
  ): java.awt.geom.AffineTransform = {
    val xscale = rect.getWidth / env.getWidth
    val yscale = rect.getHeight / env.getHeight
    val scale = math.min(xscale, yscale)
    val deltax = rect.getX - env.getMinX
    val deltay = rect.getY - env.getMinY
    val tx = new java.awt.geom.AffineTransform()
    tx.scale(scale, scale)
    tx.translate(deltax, deltay)
    tx
  }
   
  val geometryComponent = 
    new javax.swing.JComponent {
      setPreferredSize(new java.awt.Dimension(512, 512))
      
      override def paint(graphics: java.awt.Graphics) {
        import org.geotools.geometry.jts.LiteShape
        import org.geoscript.geometry._
        val envelope = 
          visibleGeometries.foldLeft(EmptyEnvelope) { 
            (e: Envelope, g: Geometry) => union(e, g.getEnvelopeInternal)
          }
        val transform = worldToScreen(getBounds(), envelope)
        val canvas = graphics.asInstanceOf[java.awt.Graphics2D]

        visibleGeometries.foreach { g =>
          val shp = new LiteShape(g, transform, true)
          if (g.getArea > 0)
            canvas.fill(shp)
          canvas.draw(shp)
        }
      }
    }

  def draw(g: geometry.Geometry) {
    synchronized {
      visibleGeometries :+= g
    }
    geometryComponent.repaint()
  }
}
