package org.geoscript.geocss

import java.io.{ File, FileInputStream, FileOutputStream, OutputStream }
import CssParser.{Success, NoSuccess}

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

  def writeSLD(style: Seq[Rule], base: java.net.URL, out: OutputStream) {
    val sld = new Translator(Option(base)).css2sld(style)
    tx.transform(sld, out)
  }

  def writeOLStyle(style: Seq[Rule], base: java.net.URL, out: OutputStream) {
    val sld = new Translator(Option(base)).css2sld(style)
    OpenLayersStyle.write(sld, out)
  }

  def writeRaw(style: Seq[Rule], base: java.net.URL, out: OutputStream) {
    val writer = new java.io.PrintWriter(out)
    style.foreach(writer.println)
    writer.close()
  }

  def main(args: Array[String]) = {
    val (options, filenames) = parse(args)

    val write: (Seq[Rule], java.net.URL, OutputStream) => Unit = 
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
      val url = in.toURI.toURL
      if (in exists) {
        val styleSheet = CssParser.parse(new FileInputStream(in))
        val out = target(in, suffix)
        styleSheet match {
          case Success(style, _) => write(style, url, out)
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
