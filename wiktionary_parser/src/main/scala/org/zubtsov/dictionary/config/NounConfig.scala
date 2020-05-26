package org.zubtsov.dictionary.config

import org.zubtsov.dictionary.parser.{NounParser, Parser}

object NounConfig extends Config{

  override def getParser: Parser = new NounParser
}
