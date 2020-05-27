package nlp.dictionary

import nlp.dictionary.zaliznyak.ZaliznyakParser

object Main extends App {
  val parser = new ZaliznyakParser(args(0))

  println(parser.matchedLines.mkString("\n"))

  println("Successses: " + parser.matchedLines.length)
  println("Failures: " + parser.numberOfErrors)
}