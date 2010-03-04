package org.geoserver.community.css

import java.io.{ File, FileInputStream, FileOutputStream, OutputStream }
import CssParser.{Success, NoSuccess}
import Translator.css2sld

object Converter {
  val tx = new org.geotools.styling.SLDTransformer
  tx.setIndentation(4)

  val Switch = """--(\p{Graph}+)=(.*)""".r
  val Flag = """--(\p{Graph}+)""".r

  def parse(args: Seq[String]): (Map[String,String], Seq[String]) = {
    val (switches, params) = args.partition(_ startsWith "--")

    val keyValuePairs = switches map {
      case Switch(key, value) => (key, value)
      case Flag(key) => (key, "")
    }

    val options = Map[String, String](keyValuePairs.toSeq: _*)

    (options, params.toSeq)
  }

  def target(f: File, suffix: String): OutputStream = {
    val rename = f.getName.replaceFirst("""\.[^.]*$""", "") + "." + suffix
    new FileOutputStream(new File(f.getParentFile, rename))
  }

  def writeSLD(style: List[Rule], out: OutputStream) {
    tx.transform(css2sld(style), out)
  }

  def writeOLStyle(style: List[Rule], out: OutputStream) {
    OpenLayersStyle.write(css2sld(style), out)
  }

  def writeRaw(style: List[Rule], out: OutputStream) {
    val writer = new java.io.PrintWriter(out)
    style.foreach(writer.println)
    writer.close()
  }

  def main(args: Array[String]) = {
    val (options, filenames) = parse(args)

    val write: (List[Rule], OutputStream) => Unit = 
      options.get("output") match {
        case Some("sld") => writeSLD
        case Some("ol-style") => writeOLStyle
        case Some("ast") => writeRaw
        case _ => writeSLD
      }
    
    val suffix = options.get("output") match {
        case Some("sld") => "sld"
        case Some("ol-style") => "ol.style"
        case Some("ast") => "ast"
        case _ => "sld"
      }

    filenames.foreach { x =>
      val in = new File(x)
      if (in exists) {
        val styleSheet = CssParser.parse(new FileInputStream(in))
        val out = target(in, suffix)
        styleSheet match {
          case Success(style, _) => write(style, out)
          case ns: NoSuccess => { 
            println("In file: %s".format(in))
            println(ns)
          }
        }
      } else {
        println("Skipping: %s [the file was not found]".format(x))
      }
    }
  }
}
