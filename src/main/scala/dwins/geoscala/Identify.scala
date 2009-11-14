package dwins.geoscala

import java.io.File
import java.net.URL
import java.util.HashMap
import org.geotools.data.DataStoreFinder

object Identify extends GeoCrunch {
  implicit def iterator[T](i: java.util.Iterator[T]): Iterator[T] = 
    new Iterator[T] {
      def next = i.next
      def hasNext = i.hasNext
    }

  def main(args: Array[String]) = {
    val shp = new File(args(0)).toURI.toURL
    val connect = new HashMap[String, Object]
    connect.put("url", shp)

    DataStoreFinder.getDataStore(connect) match {
      case null => println("Couldn't open datastore at " + shp)
      case store => store.getTypeNames.foreach(
        typename => {
          val source = store.getFeatureSource(typename)
          val featuretype = source.getSchema
          println("Schema for %s/%s".format(shp, typename))
          featuretype.getDescriptors.iterator.map(
            x => "%12s: %6s, %s".format(
              x.getName, 
              x.getType.getBinding.getSimpleName,
              x.getType.getRestrictions
            )
          ).foreach(println)
          println("Contains %s features".format(source.getFeatures.size))
        }
      )
    }
  }
}
