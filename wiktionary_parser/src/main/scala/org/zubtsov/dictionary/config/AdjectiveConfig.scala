package org.zubtsov.dictionary.config

import org.zubtsov.dictionary.parser.{AdjectiveParser, Parser}

object AdjectiveConfig extends Config {

  override def getParser: Parser = new AdjectiveParser
}
