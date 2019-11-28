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

  var formatHTML = true

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

    def fix(s: String) = {
      val t = s.trim
      if (t.contains(",")) {
        val a = t.split(",").map(_.trim)
        a(1) + " " + a(0)
      } else t
    }
    val l = for (e <- v) yield fix(e)

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


  def fixUmlaut(s: String) = {

    val m = Map("{\\o}" -> "\u00f8", "\\\"u" -> "\u00FC", "{\\'o}" -> "o", "\\'{e}" -> "\u00e9", "\\'{a}" -> "\u00e1")

    def mySplit(s: String, p: String, subst: String): String = {
      // TODO: a recursive function to split on a string
      val pos = s.indexOf(p)
      if (pos < 0) s else s.substring(0, pos) + subst + mySplit(s.substring(pos+p.length, s.length), p, subst)
    }

    var ret = s
    for ((k, v) <- m) {
      ret = mySplit(ret, k, v)
    }
    ret
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
    if (formatHTML)
      s"<h3>${y.toString}</h3>\n<ol>"
    else
      s"${y.toString}\n"
  }

  def formatEndYear() = {
    "</ol>\n"
  }


  def formatItem(map: Map[String, String]) = {

    val isArticle = map.contains("journal")

    def num(s: String) = if (formatHTML) s.split("--").mkString("-") else s

    val authors = fixUmlaut(map("author"))
    val title = map("title")
    val p = if (map.contains("pages")) map("pages") else ""
    val pages = num(p)
    val n = if (map.contains("number")) "(" + map("number") + ")" else ""
    val number = num(n)
    val volume = if (map.contains("volume")) map("volume")+ number + ":" + pages else ""

    val in = if (isArticle) "<em>" + map("journal") + "</em>, " + volume + ", " + map("year")
      // TODO: month, where, pages...
      else "<em>" + map("booktitle") + "</em>, " + map("year")

    s"<li><p> $authors\n <b>$title.</b><br>\n $in.\n</p></li>\n"
  }

  val articles = entryMap.filter(x => typeMap(x._1) == "ARTICLE")
  val paper = entryMap.filter(x => typeMap(x._1) == "INPROCEEDINGS")
  val books = entryMap.filter(x => typeMap(x._1) == "BOOK")

  val paperMin = min
  min = 2008
  var s = ""

  s += "<h2>Journal Articles</h2>"
  s += formatMonths(articles, formatYear, formatEndYear, formatItem)
  s += "<h2>Reviewed Conference and Workshop Papers</h2>"
  min = paperMin
  s += formatMonths(paper, formatYear, formatEndYear, formatItem)

  // Check is flag or different functions is the better way
  println(s)
  val pw = new PrintWriter("pub.html")
  pw.print(s)
  pw.close()
}
