package nlp.dictionary.zaliznyak.declension

import nlp.dictionary.zaliznyak.declension.types.CommonDeclensionsTable
import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber, HasStem}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasInitialForm, HasStress, HasSyntacticAndMorphologicalCharacteristics}

//aka Склонение имени
class InflectedForm {
  private val volatileVowelRule = new VolatileVowelRule()
  private val commonDeclensionsTable = new CommonDeclensionsTable()
  private val additionalDeclensionsTable = new AdditionalDeclensionsTable()
  private val specialDeclensionTable = new SpecialDeclensionTable()

  def produce(declensionParameters: HasDeclensionTypeAndSubtype with HasStem with HasGender with HasNumber with HasCase with HasAnimacy with HasStress
    with HasSyntacticAndMorphologicalCharacteristics with HasInitialForm) = {
    val ending = endingForStemWithoutVolatileVowel(declensionParameters)

    val newStem = if (declensionParameters.hasVolatileVowel) {
      volatileVowelRule.applyRule(declensionParameters, ending)
    } else {
      declensionParameters.stem
    }
    newStem + ending
  }

  def endingForStemWithoutVolatileVowel(declensionParameters: HasDeclensionTypeAndSubtype with HasStem with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.declensionSubtype
    declensionSubtype match {
      case 1 | 2 => commonDeclensionsTable.ending(declensionParameters)
      case 3 | 4 | 5 | 6 | 7 => additionalDeclensionsTable.ending(declensionParameters)
      case 8 => specialDeclensionTable.ending(declensionParameters)
      case _ => ???
    }
  }
}
