package org.zubtsov.dictionary

import org.zubtsov.dictionary.wiktionary.WiktionaryParser

object WiktionaryMain extends App {

  WiktionaryParser.parse(args(0), args(1))
}