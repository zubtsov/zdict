package org.zubtsov.dictionary.zaliznyak.declension

import org.scalatest.FunSuite
import org.zubtsov.dictionary.zaliznyak.DictRecordMapper
import org.zubtsov.dictionary.zaliznyak.entities.NameDictionaryRecord

class VolatileVowelSuite extends FunSuite {
  test("сон 2 м 1*в") {
    assertResult(
      List("сон", "сна", "сну", "сон", "сном", "сне", "сны", "снов", "снам", "сны", "снами", "снах")
    )(generateAllInflectedForms("сон 2 м 1*в"))
  }

  test("любовь 4 ж 8*в'") {
    assertResult(
      List("любовь", "любви", "любви", "любовь", "любовью", "любви", "любви", "любвей", "любвям", "любви", "любвями", "любвях")
    )(generateAllInflectedForms("любовь 4 ж 8*в'"))
  }

  test("волчий 2 п мс 6*а'") {
    assertResult(
      List("волчий", "волчьего", "волчьему", "волчьего", "волчьим", "волчьем", "волчьи", "волчьих", "волчьим", "волчьих", "волчьими", "волчьих")
    )(generateAllInflectedForms("волчий 2 п мс 6*а"))
  }

  def generateAllInflectedForms(line: String): List[String] = {
    DictRecordMapper.map(NameDictionaryRecord(line)).inflectedForms.map(_._2).toList
  }
}
