package org.geoserver.community.css.web

import org.apache.wicket.markup.html.IHeaderContributor
import org.apache.wicket.markup.html.IHeaderResponse
import org.apache.wicket.markup.html.panel.Panel

import org.geoserver.catalog.ResourceInfo

/**
 * A Wicket widget that encapsulates an OpenLayers interactive map.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
class OpenLayersMapPanel(id: String, resource: ResourceInfo) extends Panel(id)
with IHeaderContributor {
  val rand = new java.util.Random
  val bbox = resource.getLatLonBoundingBox
  setOutputMarkupId(true)

  def renderHead(response: IHeaderResponse) {
    response.renderString("""
      <style type="text/css">
        div#%1$s {
          border: 2px groove black;
          height: 300px;
        }

        div#%1$s div.olMap {
          height: 100%%;
        }
      </style>
    """.format(getMarkupId()))

    response.renderJavascriptReference(
      "../openlayers/OpenLayers.js"
    )

    response.renderOnLoadJavascript("""
      OpenLayers.DOTS_PER_INCH= 25.4 / 0.28;

      var cfg = {
        maxExtent: new OpenLayers.Bounds(%1$f, %2$f, %3$f, %4$f),
        maxResolution: %8$f,
        controls: [
          new OpenLayers.Control.PanZoomBar(),
          new OpenLayers.Control.Navigation()
        ]
      }

      var map = new OpenLayers.Map("%5$s", cfg);
      map.addLayer(new OpenLayers.Layer.WMS("GeoServer WMS", "../wms",
          {
            layers: "%6$s",
            styles: "cssdemo",
            format: "image/png",
            random: %7$d
          }
        )
      );

      map.zoomToMaxExtent();
      window.olMaps = window.olMaps || {};
      window.olMaps["%5$s"] = map;
    """.format(
        bbox.getMinX, bbox.getMinY, bbox.getMaxX, bbox.getMaxY,
        getMarkupId(),
        resource.getPrefixedName(),
        rand.nextInt(),
        bbox.getSpan(0).max(bbox.getSpan(1)) / 256.0
    ))
  }

  /*
   * Create the JavaScript snippet to execute when the map tiles should be
   * updated.
   */
  def getUpdateCommand(): String = {
  """
    var map = window.olMaps["%s"];
    for (var i = 0; i < map.layers.length; i++) {
      var layer = map.layers[i];
      if (layer.mergeNewParams) layer.mergeNewParams({random: %d});
    }
  """.format(getMarkupId(), rand.nextInt())
  }
}
