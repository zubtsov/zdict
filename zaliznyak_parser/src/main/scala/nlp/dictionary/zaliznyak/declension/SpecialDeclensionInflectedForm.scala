package nlp.dictionary.zaliznyak.declension

import nlp.dictionary.zaliznyak.declension.types.CommonDeclensionInflectedForm
import nlp.dictionary.zaliznyak.feature.common.NameWithStem
import nlp.dictionary.zaliznyak.feature.enums.common
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.{Case, DeclensionType}
import nlp.dictionary.zaliznyak.helper.Utils._

//aka Особое склонение имен
trait SpecialDeclensionInflectedForm extends CommonDeclensionInflectedForm with NameWithStem {
  def specialDeclensionEnding() = {
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
          val initialEnding = commonEndingOfSubtype2()

          if (stem.takeRight(1).isFizzingConsonant())
            initialEnding.replace("я", "а")
          else
            initialEnding
        }
      case _ => ???
    }
  }
}
