package org.geoscala.example.kmlrip

import java.net.URL

/** 
 * Rip is the main entry point to the KMLRip application.  It expects some 
 * commandline parameters or something.
 * @todo Real documentation here
 * @author David Winslow <cdwinslow@gmail.com>
 */
object Rip {
  val startUrl = new URL("""http://demo.opengeo.org/geoserver/wms?height=256&bbox=-180.0%2C-90.0%2C0.0%2C90.0&width=256&layers=topp:states&request=GetMap&service=wms&format_options=SUPEROVERLAY%3Atrue%3BKMPLACEMARK%3Afalse%3BOVERLAYMODE%3Aauto%3BKMSCORE%3A50%3BKMATTR%3Atrue%3B&srs=EPSG%3A4326&format=application%2Fvnd.google-earth.kml%2Bxml&transparent=false&version=1.1.1""")

  def main(args: Array[String]) = {
    val manager = new QueueManager(5)
    manager.documentQueue += startUrl

    manager.start
    // return Unit to keep the return value of start from confusing maven's 
    // scala:run task
    () 
  }
}
