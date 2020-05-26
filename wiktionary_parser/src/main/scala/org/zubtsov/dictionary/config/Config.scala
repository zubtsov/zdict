package org.zubtsov.dictionary.config

import org.zubtsov.dictionary.parser.Parser

trait Config {

  def getParser: Parser
}
