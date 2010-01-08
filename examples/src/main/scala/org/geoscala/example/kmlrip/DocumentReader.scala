package org.geoscala.example.kmlrip

import java.net.URL

import scala.actors.Actor
import scala.io.Source
import scala.xml.parsing.ConstructingParser

class DocumentReader(queue: QueueManager) extends Actor {
  def act = {
    queue ! Ready(this)

    while (true) {
      receive {
        case ReadUrl(url) => {read(url); queue ! Ready(this)}
        case Done => exit
      }
    }
  }
 
  def read(url: URL) = {

    val connection = url.openConnection()
    val source = Source.fromURL(url)

    val parser = ConstructingParser.fromSource(source, false)

    val doc: scala.xml.Document = try {
      parser.document
    } catch {
      case x => {
        x.printStackTrace
        null
      }
    }

    if (doc != null) {
      val valid = isGood(doc)
      val mimetype = connection.getContentType
      val length = connection.getContentLength

      queue ! Document(url, valid, mimetype, length)

      if (valid) {
        val links = doc.docElem \\ "NetworkLink" \ "Link" \ "href"
        val placemarks = doc.docElem \\ "Placemark"

        links.foreach(n => {
          val target = new URL(n.child.text)
          queue ! Link(url, target)
        }) 

        placemarks.foreach(n => {
          val id = n.attribute("id")
          if (id.isDefined) {
            queue ! Feature(url, id.get.text)
          } else { 
            println(n.attributes)
          }
        })
      } 
    }
  }

  def isGood(doc: scala.xml.Document): Boolean = {
    if (doc == null) return false

    val root = doc.docElem
    root.label == "kml" && 
    root.namespace == "http://www.opengis.net/kml/2.2"
  }
}
