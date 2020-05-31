package nlp.dictionary.zaliznyak.declension

import nlp.dictionary.zaliznyak.declension.types.CommonDeclensionInflectedForm

//aka Склонение имени
trait InflectedFormOfName extends CommonDeclensionInflectedForm with AdditionalDeclensionsInflectedForm with SpecialDeclensionInflectedForm with FormWithVolatileVowel {
  def inflectedForm() = {
    val ending = endingForStemWithoutVolatileVowel()

    val newStem = if (hasVolatileVowel) {
      applyVolatileVowelRule(ending)
    } else {
      stem
    }
    newStem + ending
  }

  private def endingForStemWithoutVolatileVowel() = {
    declensionSubtype match {
      case 1 | 2 => commonEnding()
      case 3 | 4 | 5 | 6 | 7 => additionalDeclensionEnding()
      case 8 => specialDeclensionEnding()
      case _ => ???
    }
  }
}
