package org.zubtsov.dictionary.zaliznyak

import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.DeclensionType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.DeclensionType.DeclensionType

//aka Графическая основа (не путать с основой) слова
object Stem {
  val toRemoveOneLetter = ".*[а|А|е|Е|ё|Ё|и|И|о|О|у|У|ы|Ы|э|Э|ю|Ю|я|Я|й|Й|ь|Ь]$".r
  val toRemoveNothing = ".*[б|Б|в|В|г|Г|д|Д|ж|Ж|з|З|к|К|л|Л|м|М|н|Н|п|П|р|Р|с|С|т|Т|ф|Ф|х|Х|ц|Ц|ч|Ч|ш|Ш|щ|Щ]$".r
  val toRemoveFourLastLetters = ".*ся$".r

  val toRemoveFromInfinitive = "ться|тись|чься|ть|ти|чь$"
  val toRemoveFromFirstPersonSingularPresent = "усь|юсь|у|ю$"
  val toRemoveFromThirdPersonSingularPresent = "ется|ётся|ится|ет|ёт|ит$"

  def getStem(declensionType: DeclensionType, initialForm: String): String = declensionType match {
    case DeclensionType.Substantive | DeclensionType.Pronounative => initialForm match {
      case toRemoveOneLetter() => initialForm.dropRight(1)
      case toRemoveNothing() => initialForm
    }
    case DeclensionType.Adjectival => initialForm match {
      case toRemoveFourLastLetters() => initialForm.dropRight(4)
      case _ => initialForm.dropRight(2)
    }
    case _ => ???
  }

  //todo: add reflexive parameter?
  def getStemOfInfinitive(infinitive: String) = {
    infinitive.replaceAll(toRemoveFromInfinitive, "")
  }

  def getStemOfFirstPersonSingularPresentTense(infinitive: String) = {
    infinitive.replaceAll(toRemoveFromFirstPersonSingularPresent, "")
  }

  def getStemOfThirdPersonSingularPresentTense(infinitive: String) = {
    infinitive.replaceAll(toRemoveFromThirdPersonSingularPresent, "")
  }
}
