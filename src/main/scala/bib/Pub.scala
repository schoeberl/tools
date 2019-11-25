package bib

import org.jbibtex._
import scala.io.Source
import java.io._
import collection.JavaConverters._
import collection.mutable._

object Pub extends App {
  val bibFile = "/Users/martin/paper/bib/jop_bib.bib"

  val reader = new FileReader(bibFile);
  val parser = new BibTeXParser();
  val database = parser.parse(reader);
  val entryMapJava = database.getEntries().asScala

  val entryMap = Map[String, Map[String, String]]()
  val typeMap = Map[String, String]()

  for ((k, v) <- entryMapJava) {
    val map = Map[String, String]()
    for ((fk, fv) <- v.getFields().asScala) {
      map += (fk.toString -> fv.toUserString)
    }
    entryMap += (k.toString -> map)
    typeMap += (k.toString -> v.getType.toString)
  }
  // Now we are in Scala land

  var min = 3000
  var max = 0
  for ((k, v) <- entryMap) {
    print(typeMap(k) + " ")
    print(k + " : ")
    v.foreach(print)
    println()
    val year = v("year").toInt
    min = if (year < min) year else min
    max = if (year > max) year else max
  }

  println(min)
  println(max)

  val months = List("January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December")

  for (year <- max to min by -1) {
    for (month <- months.reverse) {
      println(year + ", " + month)
    }
  }

}
