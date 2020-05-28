package nlp.dictionary.zaliznyak.declension.types

import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasStress}
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType

//aka Основные склонения имен
class CommonDeclensionsTable {
  val adjectiveDeclensionTable = new AdjectiveDeclensionTable()
  val substantiveDeclensionTable = new SubstantiveDeclensionTable()
  val pronounativeDeclensionTable = new PronounativeDeclensionTable()

  def ending(declensionParameters: HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.{declensionSubtype, declensionType}
    assert(Set(1, 2).contains(declensionSubtype))
    declensionType match {
      case DeclensionType.Adjectival => adjectiveDeclensionTable.ending(declensionParameters)
      case DeclensionType.Substantive => substantiveDeclensionTable.ending(declensionParameters)
      case DeclensionType.Pronounative => pronounativeDeclensionTable.ending(declensionParameters)
      case _ => ???
    }
  }
}
