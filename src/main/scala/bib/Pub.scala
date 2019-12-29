package bib

import org.jbibtex._
import scala.io.Source
import java.io._
import collection.JavaConverters._
import collection.mutable._

object Pub extends App {
  val bibFile = "/Users/martin/paper/bib/jop_bib.bib"
  // val bibFile = "/Users/martin/paper/tcapapers/2015/ftp-predict/web/pubs.bib"
  var formatHTML = false

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

    val m = Map("{\\o}" -> "\u00f8", "\\\"u" -> "\u00FC", "{\\'o}" -> "o", "\\'{e}" -> "\u00e9", "{\\'e}" -> "\u00e9",
      "\\'{a}" -> "\u00e1", "{\\\"o}" -> "\u00f6", "{\\\"a}" -> "\u00e4")

    def mySplit(s: String, p: String, subst: String): String = {
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
      s"\\subsubsection*{${y.toString}}\n\n"
  }

  def formatEndYear() = {
    if (formatHTML) "</ol>\n" else "\n"
  }


  def formatItem(map: Map[String, String]) = {

    val isArticle = map.contains("journal")

    def num(s: String) = if (formatHTML) s.split("--").mkString("-") else s

    val authors = fixUmlaut(map("author"))
    val title = map("title")
    val p = map.getOrElse("pages", "")
    val pages = num(p)
    val pdelim = if (pages == "") "" else ", "
    val vdelim = if (pages == "") "" else ":"
    val n = if (map.contains("number")) "(" + map("number") + ")" else ""
    val number = num(n)
    // This if contains is not very elegant, any better solution?
    val volume = if (map.contains("volume")) map("volume")+ number + vdelim + pages + ", " else ""
    val month = if (map.contains("month")) map("month") + ", " else ""
    val location = if (map.contains("location")) map("location") + ", " else ""

    val emStart = if (formatHTML) "<em>" else "\\emph{"
    val emEnd = if (formatHTML) "</em>, " else "} "

    val in = if (isArticle) emStart + map("journal") + emEnd + volume + map("year")
      else emStart + map("booktitle") + emEnd + pages + pdelim + location + month + map("year")

    // <a href="http://www.jopdesign.com/doc/jophwlocks.pdf">pdf</a>
    //        <a href="http://dx.doi.org/10.1002/cpe.3950">doi</a>
    val doi = map.getOrElse("doi", "")
    val url = map.getOrElse("url", "")
    val delim = if (doi != "" && url != "") ", " else ""

    val formatDoi = if (doi == "") "" else "<a href=\"" + s"http://dx.doi.org/$doi" + "\">doi</a>"
    val formatUrl = if (url == "") "" else "<a href=\"" + url + "\">pdf</a>"

    val links = if (doi != "" || url != "")  s"($formatDoi$delim$formatUrl)" else ""

    val htmlEntry = s"<li><p> $authors\n <b>$title.</b><br>\n $in. $links\n</p></li>\n"
    // Maybe TODO: links for PDF
    val texEntry = s"\\item $authors\n $title.\n $in.\n\n"

    if (formatHTML) htmlEntry else texEntry
  }

  val articles = entryMap.filter(x => typeMap(x._1) == "ARTICLE")
  val paper = entryMap.filter(x => typeMap(x._1) == "INPROCEEDINGS")
  val books = entryMap.filter(x => typeMap(x._1) == "BOOK")

  val paperMin = min
  min = 2008
  var s = ""


  s += (if (formatHTML) "<h2>Journal Articles</h2>\n\n" else "\\subsection*{Journal Articles}\n\n\\begin{enumerate}\n\n")
  s += formatMonths(articles, formatYear, formatEndYear, formatItem)
  s += (if (formatHTML) "" else "\\end{enumerate}\n\n")
  s += (if (formatHTML) "<h2>Reviewed Conference and Workshop Papers</h2>\n\n" else "\\subsection*{Reviewed Conference and Workshop Papers}\n\n\\begin{enumerate}\n\n")
  min = paperMin
  s += formatMonths(paper, formatYear, formatEndYear, formatItem)
  s += (if (formatHTML) "" else "\\end{enumerate}\n\n")

  // Check if flag or different functions is the better way
  println(s)
  val pw = new PrintWriter(if (formatHTML) "pub.html" else "pub.tex")
  pw.print(s)
  pw.close()
}
