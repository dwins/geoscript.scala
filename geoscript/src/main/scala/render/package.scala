package org.geoscript

import style.combinators._, feature._, geometry._, projection._

import collection.JavaConverters._
import org.{ geotools => gt }
import gt.geometry.jts.ReferencedEnvelope
import java.awt
import java.awt.RenderingHints,
  RenderingHints.{ KEY_ANTIALIASING, VALUE_ANTIALIAS_ON }

package render {
  trait Stylable[T] {
    def applyStyle(t: T, s: style.Style): Layer
    def defaultStyle(t: T): Layer
  }
  
  object Stylable {
    private val defaultSchema =
      Schema("empty", Seq(
        bind[Geometry]("the_geom", projection.LatLon)))

    def by[T, U : Stylable](f: T => U): Stylable[T] =
      new Stylable[T] {
        def applyStyle(t: T, s: style.Style): Layer =
          implicitly[Stylable[U]].applyStyle(f(t), s)

        def defaultStyle(t: T): Layer =
          implicitly[Stylable[U]].defaultStyle(f(t))
      }

    implicit val vectorDataIsStylable: Stylable[layer.Layer] =
      new Stylable[layer.Layer] {
        def applyStyle(l: layer.Layer, s: style.Style): Layer = 
          new gt.map.FeatureLayer(l, s.underlying, l.name)

        def defaultStyle(l: layer.Layer): Layer = {
          val Point = classOf[Point]
          val MultiPoint = classOf[MultiPoint]
          val LineString = classOf[LineString]
          val MultiLineString = classOf[MultiLineString]
          val sty = 
            l.schema.geometry.binding match {
              case Point | MultiPoint => Symbol("square")
              case LineString | MultiLineString => Stroke("black")
              case _ => Fill("grey") and Stroke("black")
            }
          new gt.map.FeatureLayer(l, sty.underlying, l.name)
        }
      }

    implicit val seqOfFeaturesIsStylable: Stylable[Seq[Feature]] =
      Stylable.by { fs: Seq[Feature] => 
        val schema = fs.headOption
          .map (_.getFeatureType) 
          .getOrElse(defaultSchema)

        val store = new org.geotools.data.memory.MemoryDataStore(fs.toArray)
        store.layerNamed(store.names.head)
      }

    implicit val singleFeatureIsStylable: Stylable[Feature] =
      Stylable.by { (f: Feature) => Seq(f) }

    implicit val geometryIsStylable: Stylable[Geometry] =
      Stylable.by { (g: Geometry) => defaultSchema.feature(Seq(g)) }

    implicit val seqOfGeometriesIsStylable: Stylable[Seq[Geometry]] =
      Stylable.by { (_: Seq[Geometry]).map { g =>
        defaultSchema.feature(Seq(g))
      } }
  }

  trait Canvas[T] { self =>
    def render(draw: Draw): T

    def map[U](f: T => U): Canvas[U] = new Canvas[U] {
      def render(draw: Draw): U =
        f(self.render(draw))
    }
  }

  class ImageCanvas extends Canvas[awt.image.BufferedImage] {
    val size = (256, 256) // TODO: Make this a constructor argument
    override def render(draw: Draw): awt.image.BufferedImage = {
      val (w, h) = size
      val img = new java.awt.image.BufferedImage(w, h, 
        java.awt.image.BufferedImage.TYPE_INT_ARGB)

      val graphics = img.getGraphics().asInstanceOf[java.awt.Graphics2D]
      graphics.setColor(java.awt.Color.WHITE)
      graphics.fillRect(0, 0, w, h)

      draw(graphics, size)
      graphics.dispose()
      img
    }
  }

  class Window extends Canvas[javax.swing.JFrame] {
    private val component = new javax.swing.JComponent {
      // createBufferStrategy(2)
      setBackground(java.awt.Color.WHITE)
      override def paintComponent(g: awt.Graphics) =
        draw(g.asInstanceOf[awt.Graphics2D], dimensionAsTuple(getSize()))
    }
    private val frame = new javax.swing.JFrame
    // frame.setResizable(false)
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
    frame.add(component)
    frame.setSize(256, 256)

    private var draw: Draw = { (_, _) => }

    override def render(draw: Draw): javax.swing.JFrame = {
      this.draw = draw
      component.repaint()
      if (!frame.isVisible) frame.setVisible(true)
      frame
    }
  }

  object Content {
    def empty = new Content
    def apply[T : Stylable](t: T): Content =
      empty.withData(t)
    def apply[T : Stylable](t: T, s: style.Style): Content =
      empty.withData(t, s)
  }

  class RichContent(content: Content) {
    def withLayer(l: Layer): Content = {
      val newContent = new Content
      newContent.layers.addAll(content.layers)
      newContent.addLayer(l)
      newContent
    }
    def withData[T : Stylable](data: T, s: style.Style): Content =
      withLayer(applyStyle(data, s))

    def withData[T : Stylable](data: T): Content =
      withLayer(implicitly[Stylable[T]].defaultStyle(data))
  }
}

package object render {
  type Content = gt.map.MapContent
  type Draw = (awt.Graphics2D, Dimension) => Unit
  type Dimension = (Int, Int)
  type Layer = gt.map.Layer
  type StyleLayer = gt.map.Layer
  type DirectLayer = gt.map.DirectLayer

  def applyStyle[T : Stylable](t: T, s: org.geoscript.style.Style): StyleLayer =
    implicitly[Stylable[T]].applyStyle(t, s)

  def draw[Out](
    content: Content,
    bounds: ReferencedEnvelope,
    canvas: Canvas[Out] = new ImageCanvas
  ): Out = {
    val hints = renderHints(KEY_ANTIALIASING -> VALUE_ANTIALIAS_ON)

    canvas.render { (graphics, screenArea) =>
      val renderer = new gt.renderer.lite.StreamingRenderer()
      renderer.setJava2DHints(hints)
      renderer.setMapContent(content)
      renderer.paint(graphics, tupleAsRectangle(screenArea), bounds)
    }
  }

  def drawFull[Out](
    content: Content,
    canvas: Canvas[Out] = new ImageCanvas
  ): Out = {
    def mode[A](xs: Seq[A]): Option[A] =
      if (xs.isEmpty) None
      else            Some(xs.groupBy(identity).maxBy(_._2.size)._1)

    val boundsList     = content.layers.asScala.map(_.getBounds)
    val projectionList = boundsList.map(_.getCoordinateReferenceSystem)

    val projection = mode(projectionList).getOrElse(LatLon)

    val expand = (a: Referenced[Envelope], b: Referenced[Envelope]) => 
      Referenced.envelope(for (aEnv <- a; bEnv <- b) yield aEnv || bEnv)

    val bounds = (boundsList reduceLeftOption (expand(_, _)))

    val finalBounds = bounds
      .map(Referenced.envelope(_))
      .getOrElse(new ReferencedEnvelope(EmptyEnvelope, LatLon))

    draw(content, finalBounds, canvas)
  }
    
  def file(f: String) = new java.io.File(f)

  def png(f: java.io.File): Canvas[java.io.File] = 
    new ImageCanvas().map { i =>
      javax.imageio.ImageIO.write(i, "png", f)
      f
    }

  def png(f: String): Canvas[java.io.File] = png(file(f))

  private def renderHints(kv: (RenderingHints.Key, Any)*) = 
    new RenderingHints(kv.toMap.asJava)

  def dimensionAsTuple(dim: awt.Dimension): (Int, Int) = (dim.width, dim.height)
  def tupleAsRectangle(dim: (Int, Int)): awt.Rectangle = 
    new awt.Rectangle(0, 0, dim._1, dim._2)
}
