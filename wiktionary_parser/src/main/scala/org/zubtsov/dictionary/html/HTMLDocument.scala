package org.zubtsov.dictionary.html

import scala.xml.{Elem, Node, NodeSeq}

case class HTMLDocument(xmlElem: Elem) {

  def getElementsFromArticleByTag(tag: String): NodeSeq = {
    xmlElem \\ tag
  }

  def getElementsFromNodeByTag(node: Node, tag: String*): NodeSeq = {
    if (tag.length == 1) node \\ tag(0)
    else getElementsFromNodeByTag(node \\ tag(0), tag.drop(1))

  }
  def getElementsFromNodeByTag(nodeSeq: NodeSeq, tag: Seq[String]): NodeSeq = {
    if (tag.length == 1) nodeSeq \\ tag(0)
    else getElementsFromNodeByTag(nodeSeq \\ tag(0), tag.drop(1))
  }
}
