package org.zubtsov.dictionary.zaliznyak.declension

import org.zubtsov.dictionary.zaliznyak.attributes.common.HasStem
import org.zubtsov.dictionary.zaliznyak.attributes._
import org.zubtsov.dictionary.zaliznyak.declension.types.CommonDeclensions

//aka Склонение имени
object Declension {
  def inflectedForm(declensionParameters: HasDeclensionTypeAndSubtype with HasStem with HasGender with HasNumber with HasCase with HasAnimacy with HasStress
    with HasSyntacticAndMorphologicalCharacteristics with HasInitialForm) = {
    val ending = endingForStemWithoutVolatileVowel(declensionParameters)

    val newStem = if (declensionParameters.hasVolatileVowel) {
      VolatileVowel.stemWithVolatileEnding(declensionParameters, ending)
    } else {
      declensionParameters.stem
    }
    newStem + ending
  }

  def endingForStemWithoutVolatileVowel(declensionParameters: HasDeclensionTypeAndSubtype with HasStem with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.declensionSubtype
    declensionSubtype match {
      case 1 | 2 => CommonDeclensions.ending(declensionParameters)
      case 3 | 4 | 5 | 6 | 7 => AdditionalDeclensions.ending(declensionParameters)
      case 8 => SpecialDeclension.ending(declensionParameters)
      case _ => ???
    }
  }
}
