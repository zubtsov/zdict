package nlp.dictionary.zaliznyak.feature.common

import nlp.dictionary.zaliznyak.feature.declension.{HasDeclensionTypeAndSubtype, HasInitialForm}
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType

//aka Графическая основа (не путать с основой) слова
trait NameWithStem extends HasDeclensionTypeAndSubtype with HasInitialForm {
  private val toRemoveOneLetter = ".*[а|А|е|Е|ё|Ё|и|И|о|О|у|У|ы|Ы|э|Э|ю|Ю|я|Я|й|Й|ь|Ь]$".r
  private val toRemoveNothing = ".*[б|Б|в|В|г|Г|д|Д|ж|Ж|з|З|к|К|л|Л|м|М|н|Н|п|П|р|Р|с|С|т|Т|ф|Ф|х|Х|ц|Ц|ч|Ч|ш|Ш|щ|Щ]$".r
  private val toRemoveFourLastLetters = ".*ся$".r

  def stem: String

  protected def stemOfName(): String = declensionType match {
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
}
