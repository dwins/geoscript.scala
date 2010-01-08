package org.geoscala.example.kmlrip

import java.net.URL

import scala.actors.Actor

class QueueManager(totalActors: Int) extends Actor {
  val logger = new Logger
  var documentQueue = new scala.collection.mutable.Queue[URL]
  var readerQueue = new scala.collection.mutable.Queue[Actor]
  var urlCount = 0
  var totalPlacemarks = 0

  def act() = {
    for (i <- 1 until totalActors) {
      new DocumentReader(this).start
    }

    while (true) {
      receive {
        case Document(url, valid, mime, length) => {
          logger.document(url, valid, mime, length)
        }
        case Link(source, target) => {
          logger.link(source, target)
          if (readerQueue.isEmpty) {
            documentQueue += target 
          } else {
            readerQueue.dequeue ! ReadUrl(target)
          }
        }
        case Feature(source, id) => logger.feature(source, id)
        case Ready(actor) => { 
          urlCount += 1
          if (urlCount % 100 == 0) {println("handled " + urlCount + " urls...")}

          if (documentQueue.isEmpty) {
            readerQueue += actor
            if (readerQueue.size == totalActors) {
              readerQueue.foreach(_ ! Done)
              logger.finish
              exit
            }
          } else {
            actor ! ReadUrl(documentQueue.dequeue)
          }
        }
      }
    }
  }
}
