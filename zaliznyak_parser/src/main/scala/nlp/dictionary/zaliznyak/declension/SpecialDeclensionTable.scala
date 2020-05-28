package nlp.dictionary.zaliznyak.declension

import nlp.dictionary.zaliznyak.declension.types.CommonDeclensionsTable
import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber, HasStem}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasStress}
import nlp.dictionary.zaliznyak.feature.enums.common
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.{Case, DeclensionType}
import nlp.dictionary.zaliznyak.helper.Utils._

//aka Особое склонение имен
class SpecialDeclensionTable {
  private val commonDeclensionsTable = new CommonDeclensionsTable()

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
      case common.Number.Plural =>
        if (rCase == Case.Genetive && number == common.Number.Plural)
          "ей"
        else {
          val oldDeclensionSubtype = declensionParameters.declensionSubtype
          declensionParameters.declensionSubtype = 2
          val initialEnding = commonDeclensionsTable.ending(declensionParameters)
          declensionParameters.declensionSubtype = oldDeclensionSubtype

          if (stem.takeRight(1).isFizzingConsonant())
            initialEnding.replace("я", "а")
          else
            initialEnding
        }
      case _ => ???
    }
  }
}
