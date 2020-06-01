package nlp.dictionary.zaliznyak.declension

import nlp.dictionary.zaliznyak.declension.types.CommonDeclensionInflectedForm
import nlp.dictionary.zaliznyak.feature.declension.NameWithStem
import nlp.dictionary.zaliznyak.feature.enums.common
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.{Case, DeclensionType}

//aka Дополнительные склонения имен
trait AdditionalDeclensionsInflectedForm extends CommonDeclensionInflectedForm with NameWithStem {
  def additionalDeclensionEnding() = {
    declensionSubtype match {
      case 3 => endingOfType3()
      case 4 => endingOfType4()
      case 5 => endingOfType5()
      case 6 => endingOfType6()
      case 7 => endingOfType7()
      case _ => ???
    }
  }

  private def endingOfType3() = {
    val endingOfSubtype1 = commonEndingOfSubtype1()
    val lastLetterOfStem = stem.takeRight(1)
    lastLetterOfStem match {
      case "к" | "г" | "х" => endingOfSubtype1.replace("ы", "и")
      case _ => endingOfSubtype1
    }
  }

  private def endingOfType4() = {
    val endingOfSubtype1 = commonEndingOfSubtype1()

    val lastLetterOfStem = stem.takeRight(1)

    val ending = lastLetterOfStem match {
      case "ж" | "ш" | "ч" | "щ" => {
        val ending = endingOfSubtype1.replace("ы", "и")
        if (!isEndingStressed) ending.replace("о", "е")
        else ending
      }
      case _ => endingOfSubtype1
    }

    (declensionType, rCase, number) match {
      case (DeclensionType.Substantive, Case.Genetive, Number.Plural) => {
        gender match {
          case Gender.Masculine => "ей" //todo: добавить проверку окончания на равенство "ов", "ёв" или "ев"?
          case Gender.Feminine | Gender.Neuter => if (isEndingStressed) "ей" else "" //todo: добавить проверку нулевого окончания?
          case _ => ???
        }
      }
      case _ => ending
    }
  }

  private def endingOfType5() = {
    val endingOfType1 = commonEndingOfSubtype1()
    if (stem.endsWith("ц") && !isEndingStressed)
      endingOfType1.replace("о", "е")
    else endingOfType1
  }

  private def endingOfType6() = {
    val endingOfSubtype2 = commonEndingOfSubtype2()

    val lastLetterOfStem = stem.takeRight(1)
    val ending = lastLetterOfStem match {
      case "а" | "е" | "ё" | "о" | "у" | "ы" | "э" | "ю" | "я" | "и" | "й" => endingOfSubtype2.replace("ь", "й")
      case _ => endingOfSubtype2
    }

    (declensionType, rCase, number) match {
      case (DeclensionType.Substantive, Case.Genetive, common.Number.Plural) =>
        gender match {
          case Gender.Masculine => if (isEndingStressed) "ёв" else "ев" //todo: добавить проверку окончания на равенство "ей"?
          case Gender.Feminine | Gender.Neuter => "й" //todo: добавить проверку окончания на равенство "ь", "ей"?
          case _ => ???
        }
      case _ => ending
    }
  }

  private def endingOfType7() = {
    val endingOfSubtype2 = commonEndingOfSubtype2()

    val lastLetterOfStem = stem.takeRight(1)
    val ending = lastLetterOfStem match {
      case "и" => endingOfSubtype2.replace("ь", "й")
      case _ => endingOfSubtype2
    }

    declensionType match {
      case DeclensionType.Substantive => (rCase, number, gender) match {
        case (Case.Genetive, common.Number.Plural, Gender.Masculine) => if (isEndingStressed) "ёв" else "ев" //todo: добавить проверку окончания на равенство "ей"?
        case (Case.Genetive, common.Number.Plural, Gender.Feminine) | (Case.Genetive, common.Number.Plural, Gender.Neuter) => "й" //todo: добавить проверку окончания на равенство "ь", "ей"?
        case (Case.Dative, common.Number.Singular, Gender.Feminine) | (Case.Prepositional, common.Number.Singular, _) => if (isEndingStressed) "е" else "и" //todo: добавить проверку окончания на равенство "е"?
        case _ => ending
      }
    }
  }
}
