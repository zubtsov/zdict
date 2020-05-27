package nlp.dictionary

import nlp.dictionary.zaliznyak.ZaliznyakParser

object Main extends App {
  new ZaliznyakParser(args(0))
    .parse()
    .printFailures()
    .printSuccesses()
}