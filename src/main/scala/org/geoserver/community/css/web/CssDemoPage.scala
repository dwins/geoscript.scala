package org.geoserver.community.css.web

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.FileWriter

import scala.io.Source

import org.geoserver.catalog.FeatureTypeInfo
import org.geoserver.catalog.ResourceInfo
import org.geoserver.web.GeoServerBasePage
import org.vfny.geoserver.global.GeoserverDataDirectory

import org.geotools.data.FeatureSource

import org.opengis.feature.simple.SimpleFeature
import org.opengis.feature.simple.SimpleFeatureType

import org.apache.wicket.PageParameters
import org.apache.wicket.WicketRuntimeException
import org.apache.wicket.ajax.AjaxRequestTarget
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior
import org.apache.wicket.ajax.form.AjaxFormValidatingBehavior
import org.apache.wicket.ajax.markup.html.form.AjaxButton
import org.apache.wicket.extensions.ajax.markup.html.tabs.AjaxTabbedPanel
import org.apache.wicket.extensions.markup.html.tabs.AbstractTab
import org.apache.wicket.extensions.markup.html.tabs.ITab
import org.apache.wicket.extensions.markup.html.tabs.PanelCachingTab
import org.apache.wicket.markup.html.basic.Label
import org.apache.wicket.markup.html.form.DropDownChoice
import org.apache.wicket.markup.html.form.Form
import org.apache.wicket.markup.html.form.IChoiceRenderer
import org.apache.wicket.markup.html.form.SubmitLink
import org.apache.wicket.markup.html.form.TextArea
import org.apache.wicket.markup.html.panel.EmptyPanel
import org.apache.wicket.markup.html.panel.Panel
import org.apache.wicket.model.CompoundPropertyModel
import org.apache.wicket.model.IModel
import org.apache.wicket.model.Model
import org.apache.wicket.model.PropertyModel
import org.apache.wicket.validation.{IValidator, IValidatable, ValidationError}
import org.apache.wicket.util.time.Duration

import org.geoserver.community.css.CssParser._
import org.geoserver.community.css.Translator._

trait CssDemoConstants {
  val styleName = "cssdemo"
  val defaultStyle = """ * {
  fill: lightgrey;
  stroke: black;
  mark: symbol(square);
}"""

  private def styleSheetXML(stylesheet: List[Rule]): String = {
    val style = css2sld(stylesheet)
    val sldBytes = new java.io.ByteArrayOutputStream
    val xform = new org.geotools.styling.SLDTransformer
    xform.setIndentation(2)
    xform.transform(style, sldBytes)
    sldBytes.toString
  }

  def cssText2sldText(css: String): Either[NoSuccess, String]= {
    parse(css) match {
      case Success(rules, in) => Right(styleSheetXML(rules))
      case ns: NoSuccess => Left(ns)
    }
  }
}

class CssValidator extends IValidator {
  override def validate(text: IValidatable) = {
    text.getValue() match {
      case css: String => {
        parse(css) match {
          case ns: NoSuccess => {
            val errorMessage = 
              "Line %d, column %d: %s".format(
                ns.next.pos.line,
                ns.next.pos.column,
                ns.msg
              )
            text.error(new ValidationError().setMessage(errorMessage))
          }
          case _ => ()
        }
      } 
      case _ => text.error(new ValidationError().setMessage("CSS text must not be empty"))
    }
  }
}

/**
 * A Wicket page using the GeoServer extension system.  It adds a simple form
 * and an OpenLayers map that can be used to try out MSS styling interactively
 * with the topp:states dataset included in the default GeoServer configuration.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
class CssDemoPage(params: PageParameters) extends GeoServerBasePage
with CssDemoConstants {

  class UpdatingTextArea(id: String, m: IModel) extends TextArea(id, m) {
    add(new AjaxFormComponentUpdatingBehavior("onblur") {
      override def onUpdate(target: AjaxRequestTarget) = {
        target.addComponent(getFeedbackPanel())
      }
    })
  }


  def this() = this(new PageParameters)
  def catalog = getCatalog

  def sldText = {
    val filename = styleInfo.getFilename()
    val file = GeoserverDataDirectory.findStyleFile(filename)
    Source.fromFile(file).getLines.reduceLeft {
      (x: String, y: String) => x + y
    }
  }

  class StylePanel(id: String, model: IModel) extends Panel(id, model) {
    var styleBody = {
      val file = GeoserverDataDirectory.findStyleFile(cssSource)
      if (file != null && file.exists) {
        Source.fromFile(file).getLines.reduceLeft {
          (x: String, y: String) => x + y
        }
      } else {
        defaultStyle
      }
    }

    val sldModel: IModel = 
      new org.apache.wicket.model.AbstractReadOnlyModel {
        override def getObject() = sldText
      }

    val sldPreview = new Label("sld-preview", sldModel)

    sldPreview.setOutputMarkupId(true)
    add(sldPreview)

    val styleEditor = new Form("style-editor")
    styleEditor.add(new Label("label", "The stylesheet for this map..."))
    val textArea =
      new UpdatingTextArea("editor", new PropertyModel(this, "styleBody"))
    textArea.add(new CssValidator)
    styleEditor.add(textArea)
    styleEditor.add(new AjaxButton("submit", styleEditor) {
      override def onSubmit(target: AjaxRequestTarget, form: Form) = {
        try {
          val file = GeoserverDataDirectory.findStyleFile(cssSource, true)

          cssText2sldText(styleBody) match {
            case Left(noSuccess) => println(noSuccess.toString)
            case Right(sld) => {
              val writer = new FileWriter(file)
              writer.write(styleBody)
              writer.close()
              getCatalog.getResourcePool.writeStyle(
                styleInfo,
                new ByteArrayInputStream(sld.getBytes)
              )
            }
          }
        } catch {
          case e => throw new WicketRuntimeException(e);
        }

        getCatalog.save(styleInfo)

        target.addComponent(sldPreview)
        target.appendJavascript(map.getUpdateCommand())
      }
    })

    AjaxFormValidatingBehavior.addToAllFormComponents(styleEditor, "onkeyup", Duration.ONE_SECOND)
    add(styleEditor)
  }

  class DataPanel(id: String, model: IModel) extends Panel(id, model) {
    add(new Label(
      "summary-message",
      "For reference, here is a listing of the attributes in this data set."
    ))

    val states =
      new SummaryProvider(
        statesInfo.getFeatureSource(null,null)
        .asInstanceOf[FeatureSource[SimpleFeatureType, SimpleFeature]]
        .getFeatures
      )
    add(new SummaryTable("summary", states))
  }

  val styleInfo = getCatalog.getStyleByName(styleName)
  var statesInfo = {
    def res(a: String, b: String) =
      catalog.getResourceByName(a, b, classOf[FeatureTypeInfo])

    if (params.containsKey("layer")) {
      val name = params.getString("layer").split(":")
      res(name(0), name(1))
    } else {
      val states =
        catalog.getResourceByName("topp", "states", classOf[FeatureTypeInfo])

      if (states != null) {
        states
      } else {
        catalog.getResources(classOf[FeatureTypeInfo]).get(0)
      }
    }
  }

  def cssSource = styleInfo.getFilename.replaceAll("\\.sld$","") + ".css"

  val layerSelectionForm = new Form("layer-selection")
  layerSelectionForm.add(
    new DropDownChoice(
      "layername",
      new PropertyModel(this, "statesInfo"),
      getCatalog.getResources(classOf[FeatureTypeInfo]),
      new IChoiceRenderer {
        override def getDisplayValue(choice: AnyRef) = {
          val resource = choice.asInstanceOf[ResourceInfo]
          val layers = getCatalog.getLayers(resource)
          if (layers != null && layers.size > 0) {
            "%s [%s]".format(layers.get(0).getName, resource.getPrefixedName)
          } else {
            choice.asInstanceOf[ResourceInfo].getPrefixedName
          }
        }

        override def getIdValue(choice: AnyRef, index: Int) =
          choice.asInstanceOf[ResourceInfo].getId
      }
    )
  )

  layerSelectionForm.add(new SubmitLink("submit", layerSelectionForm) {
      override def onSubmit() = {
        val params = new org.apache.wicket.PageParameters
        params.put("layer", statesInfo.getPrefixedName)

        setResponsePage(classOf[CssDemoPage], params)
      }
    }
  )

  add(layerSelectionForm)

  val map = new OpenLayersMapPanel("map", statesInfo)
  add(map)

  val feedback2 = new org.apache.wicket.markup.html.panel.FeedbackPanel("feedback-low")
  feedback2.setOutputMarkupId(true)
  add(feedback2)

  val tabs = new java.util.ArrayList[ITab]
  val model = new CompoundPropertyModel(this)
  tabs.add(new PanelCachingTab(new AbstractTab(new Model("Style")) {
    override def getPanel(id: String): Panel = new StylePanel(id, model)
  }))
  tabs.add(new PanelCachingTab(new AbstractTab(new Model("Data")) {
    override def getPanel(id: String): Panel = new DataPanel(id, model)
  }))

  add(new AjaxTabbedPanel("tabs", tabs))
}
