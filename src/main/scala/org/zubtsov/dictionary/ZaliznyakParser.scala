package org.zubtsov.dictionary

import scala.io.Source

object ZaliznyakParser extends App {
  val wordDefinition = raw"^([а-яА-Я]+)\s+([0-9]+)\s+(м|ж|с|мо|жо|со|мо\-жо|св|нсв|св\-нсв|п)\s+[\w\W]*".r

  Source.fromFile(args(0), "Windows-1251").getLines().foreach(line => {
    line match {
      case wordDefinition(initialForm, _, modifier) => {
        println(s"Initial form: $initialForm, modifier: $modifier")
      }
      case _ =>
    }
  })
}