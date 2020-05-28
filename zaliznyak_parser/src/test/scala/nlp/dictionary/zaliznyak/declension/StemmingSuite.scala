package nlp.dictionary.zaliznyak.declension

import nlp.dictionary.zaliznyak.Stemmer
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType
import org.scalatest.FunSuite

class StemmingSuite extends FunSuite {
  test("Stemming test") {
    Seq(
      ("жо", "акула", "акул"),
      ("ж", "лыжня", "лыжн"),
      ("с", "ведро", "ведр"),
      ("с", "бельё", "бель"),
      ("м", "край", "кра"),
      ("м", "тополь", "топол"),
      ("мс", "лисий", "лиси"),
      ("м", "поезд", "поезд"),
      ("м", "стол", "стол"),
      ("мс", "дядин", "дядин"),
      ("мс", "отцов", "отцов"),
      ("п", "белый", "бел"),
      ("п", "синий", "син"),
      ("п", "часовой", "часов"),
      ("п", "запятая", "запят"),
      ("п", "лёгкое", "лёгк"),
      ("п", "пресмыкающееся", "пресмыкающ")
    ).foreach(t => assertResult(t._3)(new Stemmer().getStem(DeclensionType(t._1), t._2)))
  }
}