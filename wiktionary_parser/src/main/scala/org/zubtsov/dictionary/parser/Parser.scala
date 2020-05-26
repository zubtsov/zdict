package org.zubtsov.dictionary.parser

import org.zubtsov.dictionary.entity.Lexeme

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node, NodeSeq}

trait Parser {

  def parse(xmlFile: Elem, word: String): ListBuffer[Lexeme]

  def getCasesTable(xmlFile: Elem): Seq[Node] = {
      for {
        table <- xmlFile \\ "tbody"
        tableHeader = table \\ "tr" \\ "th"
        if isCasesTable(tableHeader)
      } yield table
  }

  def isCasesTable(seq: NodeSeq): Boolean

}
