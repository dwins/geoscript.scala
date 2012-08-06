package org.geoscript.example 

object Render extends App {
  import org.geoscript._, render._

  def reference(e: org.geoscript.geometry.Envelope, p: projection.Projection) = 
    new org.geotools.geometry.jts.ReferencedEnvelope(e, p)

  val Array(dataFile, styleFile) = 
    if (args.size > 1) args.take(2)
    else Array("../geoscript/src/test/resources/data/states.shp",
               "../geocss/src/test/resources/states.css")

  val states = layer.Shapefile(dataFile)
  val theme = style.CSS.fromFile(styleFile)
  val bounds = reference(states.envelope, projection.Projection("EPSG:4326"))
  val win = new org.geoscript.render.Window
  draw(Content(states, theme), Stretch(bounds), win)

  val watcher = new actors.DaemonActor {
    val styleFile = new java.io.File("../geocss/src/test/resources/states.css")
    var updated = styleFile.lastModified
    override def act = loop {
      Thread.sleep(1000)
      val lastModified = styleFile.lastModified
      if (updated < lastModified) {
        try {
          val theme = style.CSS.fromFile("../geocss/src/test/resources/states.css")
          draw(Content(states, theme), Stretch(bounds), win)
        } catch {
          case _ => ()
        }
      }
      updated = lastModified
    }
  }
  watcher.start()
}
