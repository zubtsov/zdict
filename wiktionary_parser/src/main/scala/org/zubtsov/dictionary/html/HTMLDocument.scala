package org.zubtsov.dictionary.html

import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.JavaConverters._
import scala.collection.mutable

case class HTMLDocument(document: Document) {

  def getElementsFromArticleByTag(tag: String) = {
    asScalaBuffer(document.body().getElementsByTag(tag))
  }

  def getElementsFromNodeByTag(element: Element, tag: Seq[String]): mutable.Buffer[Element] = {
    if (tag.length == 1) asScalaBuffer(element.getElementsByTag(tag(0)))
    else getElementsFromNodeByTag(element.getElementsByTag(tag(0)), tag.drop(1))
  }

  def getElementsFromNodeByTag(element: Element, tag: String) = {
    asScalaBuffer(element.getElementsByTag(tag))
  }

  def getElementsFromNodeByTag(elements: Elements, tag: Seq[String]): mutable.Buffer[Element] = {
    if (tag.length == 1) asScalaBuffer(elements.select(tag(0)))
    else getElementsFromNodeByTag(elements.select(tag(0)), tag.drop(1))
  }

  def getChildElements(element: Element) ={
    asScalaBuffer(element.children())
  }

  def getAttributeValue(element: Element, attributeKey: String): String ={
    element.attr(attributeKey)
  }


}
