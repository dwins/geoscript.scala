package org.geoscript

import style.combinators._, feature._, geometry._, projection._, serialize._

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
      Schema("empty", "the_geom".binds[Geometry])

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
          new gt.map.FeatureLayer(l, s, l.name)

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
        store(store.names.head)
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

  class ImageCanvas(size: (Int, Int) = (256, 256)) extends Canvas[awt.image.BufferedImage] {
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
    /**
      * Calculate a bounding box for this Content that will include all data 
      * from all layers, in the projection used by the largest fraction of
      * them.
      */
    def calculatedBounds: ReferencedEnvelope = {
      def freqs[A](xs: Seq[A]): Map[A, Int] =
        (Map.empty[A, Int] /: xs) { 
          (accum, x) => accum + ((x, accum.getOrElse(x, 0) + 1))
        }

      def mode[A](xs: Seq[A]): Option[A] =
        if (xs isEmpty)
          None
        else
          Some(freqs(xs).toSeq.maxBy(_._2)._1)

      val expand = (a: Referenced[Envelope], b: Referenced[Envelope]) =>
        for (aEnv <- a; bEnv <- b) yield aEnv || bEnv

      val projection = 
        mode(content.layers.asScala.map(_.getBounds.getCoordinateReferenceSystem))
          .getOrElse(LatLon)
      val boundsList = content.layers.asScala.map(_.getBounds)
      val bounds = boundsList
        .map(x => x: Referenced[Envelope])
        .reduceLeftOption { (refA, refB) =>
           for { a <- refA; b <- refB } yield a || b }

      bounds match {
        case Some(bounds) => new ReferencedEnvelope(bounds.project(projection), projection)
        case None => new ReferencedEnvelope(EmptyEnvelope, projection)
      }
    }

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
  type Color = java.awt.Color
  type Draw = (awt.Graphics2D, Dimension) => Unit
  type Dimension = (Int, Int)
  type Layer = gt.map.Layer
  type StyleLayer = gt.map.Layer
  type DirectLayer = gt.map.DirectLayer

  val White = java.awt.Color.WHITE
  val Black = java.awt.Color.BLACK
  val Transparent = new java.awt.Color(0, 0, 0, 0)

  private def mkChart(geoms: Traversable[_ <: Geometry]) = {
    import org.geotools.renderer.chart. { GeometryDataset, GeometryRenderer } 
    import org.jfree.chart

    val data = new GeometryDataset(geoms.toSeq: _*)
    val renderer = new GeometryRenderer()
    val xyplot = new chart.plot.XYPlot(data, data.getDomain(), data.getRange(), renderer)
    new chart.JFreeChart(xyplot)
  }

  def plot(geoms: Traversable[_ <: Geometry]) {
    val chart = mkChart(geoms)
    val chartPanel = new org.jfree.chart.ChartPanel(chart)

    val frame = new javax.swing.JFrame()
    frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE)
    frame.setContentPane(chartPanel)
    frame.setSize(500, 500)
    frame.setVisible(true)
  }

  def plotOn[Out]
    (geoms: Traversable[_ <: Geometry],
     canvas: Canvas[Out] = new ImageCanvas)
  : Out = {
    val chart = mkChart(geoms)
    canvas.render { (graphics, screenArea) => 
      chart.draw(graphics, tupleAsRectangle(screenArea))
    }
  }

  def applyStyle[T : Stylable](t: T, s: org.geoscript.style.Style): StyleLayer =
    implicitly[Stylable[T]].applyStyle(t, s)

  type Frame = (Content, Dimension) => ReferencedEnvelope
  val AutoStretch: Frame = (content, dimension) => content.calculatedBounds
  def Stretch(re: Referenced[Envelope]): Frame = (_, _) => {
    val prj = re.native.getOrElse(LatLon)
    new ReferencedEnvelope(re project prj, prj)
  }

  def draw[Out](
    content: Content,
    frame: Frame = AutoStretch,
    // bounds: Referenced[Envelope],
    canvas: Canvas[Out] = new ImageCanvas
  ): Out = {
    val hints = renderHints(KEY_ANTIALIASING -> VALUE_ANTIALIAS_ON)
    // val proj = bounds.native getOrElse projection.LatLon
    // val refEnv = new ReferencedEnvelope(bounds.project(proj), proj)

    canvas.render { (graphics, screenArea) =>
      val refEnv = frame(content, screenArea)
      val renderer = new gt.renderer.lite.StreamingRenderer()
      renderer.setJava2DHints(hints)
      renderer.setMapContent(content)
      renderer.paint(graphics, tupleAsRectangle(screenArea), refEnv)
    }
  }

  // def drawFull[Out](
  //   content: Content,
  //   canvas: Canvas[Out] = ImageCanvas
  // ): Out = {
  //   def mode[A](xs: Seq[A]): Option[A] =
  //     if (xs.isEmpty) None
  //     else            Some(xs.groupBy(identity).maxBy(_._2.size)._1)

  //   val boundsList     = content.layers.asScala.map(_.getBounds)
  //   val projectionList = boundsList.map(_.getCoordinateReferenceSystem)

  //   val projection = mode(projectionList).getOrElse(LatLon)

  //   val expand = (a: Referenced[Envelope], b: Referenced[Envelope]) => 
  //     Referenced.envelope(for (aEnv <- a; bEnv <- b) yield aEnv || bEnv)

  //   val bounds = (boundsList reduceLeftOption (expand(_, _)))

  //   val finalBounds = bounds
  //     .map(Referenced.envelope(_))
  //     .getOrElse(new ReferencedEnvelope(EmptyEnvelope, LatLon))

  //   draw(content, finalBounds, canvas)
  // }
    
  def file(f: String) = new java.io.File(f)

  def png[Spec, Out]
    (dest: Spec = (), size: (Int, Int) = (512, 512), background: Color = Transparent)
    (implicit encodable: Encodable[Spec, Out])
    : Canvas[Out]
  = new ImageCanvas(size).map {
     img => encodable.encode(dest) {
       out => javax.imageio.ImageIO.write(img, "png", out) } }

  def png(f: String): Canvas[java.io.File] = png(file(f))

  private def renderHints(kv: (RenderingHints.Key, Any)*) = 
    new RenderingHints(kv.toMap.asJava)

  def dimensionAsTuple(dim: awt.Dimension): (Int, Int) = (dim.width, dim.height)
  def tupleAsRectangle(dim: (Int, Int)): awt.Rectangle = 
    new awt.Rectangle(0, 0, dim._1, dim._2)
}
