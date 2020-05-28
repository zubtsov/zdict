package nlp.dictionary.zaliznyak.declension

import nlp.dictionary.zaliznyak.declension.types.CommonDeclensionsTable
import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber, HasStem}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasStress}
import nlp.dictionary.zaliznyak.feature.enums.common
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.{Case, DeclensionType}

//aka Дополнительные склонения имен
class AdditionalDeclensionsTable {
  val commonDeclensionsTable = new CommonDeclensionsTable()

  def ending(declensionParameters: HasDeclensionTypeAndSubtype with HasStem with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.declensionSubtype
    assert(List(3, 4, 5, 6, 7).contains(declensionSubtype))
    declensionSubtype match {
      case 3 => endingOfType3(declensionParameters)
      case 4 => endingOfType4(declensionParameters)
      case 5 => endingOfType5(declensionParameters)
      case 6 => endingOfType6(declensionParameters)
      case 7 => endingOfType7(declensionParameters)
      case _ => ???
    }
  }

  private def endingOfType3(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.stem

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 1
    val endingOfSubtype1 = commonDeclensionsTable.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype
    val lastLetterOfStem = stem.takeRight(1)
    lastLetterOfStem match {
      case "к" | "г" | "х" => endingOfSubtype1.replace("ы", "и")
      case _ => endingOfSubtype1
    }
  }

  private def endingOfType4(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 1
    val endingOfSubtype1 = commonDeclensionsTable.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype

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

  private def endingOfType5(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 1
    val endingOfType1 = commonDeclensionsTable.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype
    if (stem.endsWith("ц") && !isEndingStressed)
      endingOfType1.replace("о", "е")
    else endingOfType1
  }

  private def endingOfType6(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 2
    val endingOfSubtype2 = commonDeclensionsTable.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype

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

  private def endingOfType7(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 2
    val endingOfSubtype2 = commonDeclensionsTable.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype

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
