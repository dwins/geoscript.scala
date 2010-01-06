package dwins.geoscala

import scala.collection.jcl.HashMap
import com.vividsolutions.jts.geom.Geometry
import org.geotools.data.DataStoreFinder
import org.opengis.feature.simple.SimpleFeature

object AllValid extends GeoCrunch {
  def main(args: Array[String]) = {
    val file = promptShapeFile
    val params = (new HashMap + ("url" -> file.toURI.toURL)).underlying
    val dataStore = DataStoreFinder.getDataStore(params)
    val typeName = dataStore.getTypeNames()(0)
    val featureSource = dataStore.getFeatureSource(typeName)

    var invalid = Nil: List[SimpleFeature]

    foreach (featureSource.getFeatures) { feature => 
      if (!feature.getDefaultGeometry.asInstanceOf[Geometry].isValid) 
        invalid ::= feature
    }
    
    println("Found %1$s invalid features.  That's %1$s too many.".format(invalid.length))
    invalid.foreach(x => println(x.getID))
  }
}
