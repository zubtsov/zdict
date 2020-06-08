package org.zubtsov.dictionary.zaliznyak.declension

import org.zubtsov.dictionary.zaliznyak.Utils.RussianLetter
import org.zubtsov.dictionary.zaliznyak.attributes.common.HasStem
import org.zubtsov.dictionary.zaliznyak.attributes.enums.{Case, DeclensionType, Gender, Number}
import org.zubtsov.dictionary.zaliznyak.attributes._
import org.zubtsov.dictionary.zaliznyak.declension.types.CommonDeclensions

//aka Особое склонение имен
object SpecialDeclension {
  def ending(declensionParameters: HasDeclensionTypeAndSubtype with HasStem with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._
    assert(declensionType == DeclensionType.Substantive) //todo: только у существительных бывает Substantive declension?
    number match {
      case Number.Singular => rCase match {
        case Case.Nominative | Case.Accusative => "ь"
        case Case.Genetive | Case.Dative | Case.Prepositional => "и"
        case Case.Instrumental => gender match {
          case Gender.Feminine => "ью"
          case Gender.Masculine => "ем"
          case Gender.Neuter => if (isEndingStressed) "ём" else "ем"
          case _ => ???
        }
        case _ => ???
      }
      case Number.Plural =>
        if (rCase == Case.Genetive && number == Number.Plural)
          "ей"
        else {
          val oldDeclensionSubtype = declensionParameters.declensionSubtype
          declensionParameters.declensionSubtype = 2
          val initialEnding = CommonDeclensions.ending(declensionParameters)
          declensionParameters.declensionSubtype = oldDeclensionSubtype

          if (stem.takeRight(1).fizzingConsonant())
            initialEnding.replace("я", "а")
          else
            initialEnding
        }
      case _ => ???
    }
  }
}