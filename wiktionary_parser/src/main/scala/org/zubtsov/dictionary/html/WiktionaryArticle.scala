package org.zubtsov.dictionary.html

import scala.xml.{Elem, Node, NodeSeq}

//todo: think about name
case class WiktionaryArticle(htmlDocument: Elem){

  def getElementsFromArticleByTag(tag: String): NodeSeq = {
    htmlDocument \\ tag
  }

  def getElementsFromNodeByTag(node: Node, tag: String): NodeSeq = {
    node \\ tag
  }
}
