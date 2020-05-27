package nlp.dictionary.zaliznyak.declension.types

import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasStress}
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType

//aka Основные склонения имен
object CommonDeclensions {
  def ending(declensionParameters: HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.{declensionSubtype, declensionType}
    assert(Set(1, 2).contains(declensionSubtype))
    declensionType match {
      case DeclensionType.Adjectival => AdjectiveDeclension.ending(declensionParameters)
      case DeclensionType.Substantive => SubstantiveDeclension.ending(declensionParameters)
      case DeclensionType.Pronounative => PronounativeDeclension.ending(declensionParameters)
      case _ => ???
    }
  }
}
