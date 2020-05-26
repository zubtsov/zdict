package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.Lexeme

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, NodeSeq}

class AdjectiveParser extends Parser {

  override def parse(xmlFile: Elem, word: String): ListBuffer[Lexeme] = ???

  override def isCasesTable(thNodes: NodeSeq): Boolean = ???

}
