package org.zubtsov.dictionary.config

import org.zubtsov.dictionary.parser.{Parser, VerbParser}

object VerbConfig extends Config{

  override def getParser: Parser = new VerbParser
}
