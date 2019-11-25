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
      map += (fk.toString -> fv.toUserString.split('\n').mkString(" "))
    }
    entryMap += (k.toString -> map)
    typeMap += (k.toString -> v.getType.toString)
  }
  // Now we are in Scala land

  def printMap(map:  Map[String, Map[String, String]]) = {
    for ((k, v) <- map) {
      print(typeMap(k) + " ")
      print(k + " : ")
      v.foreach(print)
      println()
    }
  }

  def authors(s: String) = {
    val v = s.split(" and ")
    val l = for (e <- v) yield e.trim
    val ret = if (l.length > 2)  l.init.mkString(", ") + ", and " + l.last
    else if (l.length == 2) l.head + " and " + l.last
    else l.head
    ret + "."
  }
  var min = 3000
  var max = 0
  for ((k, v) <- entryMap) {
    val year = v("year").toInt
    min = if (year < min) year else min
    max = if (year > max) year else max
    v("author") = authors(v("author"))
    v("title") = v("title").filter(_ != '}').filter(_ != '{')
  }

  def formatMonths(map:  Map[String, Map[String, String]], formatYear: (Int) => String, formatEndYear: () => String,
                   format: (Map[String, String]) => String) = {
    var s = ""

    val months = List("January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December")

    for (year <- max to min by -1) {
      s += formatYear(year)
      val paper = map.filter(x => x._2("year") == year.toString)
      val m = paper.filter(x => x._2.contains("month"))
      for (month <- months.reverse) {
        for ((k, v) <- m.filter(x => x._2("month") == month)) {
          s += format(v)
        }
      }
      val nm = paper.filter(x => !x._2.contains("month"))
      for ((k, v) <- nm) {
        s += format(v)
      }
      s+= formatEndYear()
    }
    s
  }

  def formatYear(y: Int) = {
    y.toString + "\n"
  }
  def formatEndYear() = {
    "\n"
  }


  def formatItem(map: Map[String, String]) = {

    val isArticle = map.contains("journal")

    val authors = map("author")
    val title = map("title")
    val pages = if (map.contains("pages")) map("pages") + "," else ""
    val volume = if (map.contains("volume")) map("volume")+":" + pages else ""

    val in = if (isArticle) "<em> " + map("journal") + "</em>, " + volume
      else "xxx"

    s"<li> $authors\n <b>$title.</b><br>\n $in\n</li>\n"
  }

  val articles = entryMap.filter(x => typeMap(x._1) == "ARTICLE")
  val paper = entryMap.filter(x => typeMap(x._1) == "INPROCEEDINGS")
  val books = entryMap.filter(x => typeMap(x._1) == "BOOK")
  val s = formatMonths(articles, formatYear, formatEndYear, formatItem)
  // TODO: do Latex to HTML for special characters
  println(s)
}
