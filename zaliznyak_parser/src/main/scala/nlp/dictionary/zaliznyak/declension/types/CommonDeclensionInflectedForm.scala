package nlp.dictionary.zaliznyak.declension.types

import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType

//aka Основные склонения имен
trait CommonDeclensionInflectedForm extends AdjectiveDeclensionInflectedForm with SubstantiveDeclensionInflectedForm with PronounativeDeclensionInflectedForm {
  def commonEnding() = {
    declensionType match {
      case DeclensionType.Adjectival => adjectiveEnding()
      case DeclensionType.Substantive => substantiveEnding()
      case DeclensionType.Pronounative => pronounativeEnding()
      case _ => ???
    }
  }

  def commonEndingOfSubtype1() = {
    declensionType match {
      case DeclensionType.Adjectival => adjectiveEndingOfSubtype1()
      case DeclensionType.Substantive => substantiveEndingOfSubtype1()
      case DeclensionType.Pronounative => pronounativeEndingOfSubtype1()
      case _ => ???
    }
  }

  def commonEndingOfSubtype2() = {
    declensionType match {
      case DeclensionType.Adjectival => adjectiveEndingOfSubtype2()
      case DeclensionType.Substantive => substantiveEndingOfSubtype2()
      case DeclensionType.Pronounative => pronounativeEndingOfSubtype2()
      case _ => ???
    }
  }
}
