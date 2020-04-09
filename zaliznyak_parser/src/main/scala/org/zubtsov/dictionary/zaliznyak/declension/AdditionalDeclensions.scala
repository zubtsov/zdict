package org.zubtsov.dictionary.zaliznyak.declension

import org.zubtsov.dictionary.zaliznyak.attributes._
import org.zubtsov.dictionary.zaliznyak.attributes.common.HasStem
import org.zubtsov.dictionary.zaliznyak.attributes.enums.{Case, DeclensionType, Gender, Number}
import org.zubtsov.dictionary.zaliznyak.declension.types.CommonDeclensions

//aka Дополнительные склонения имен
object AdditionalDeclensions {
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

  def endingOfType3(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters.stem

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 1
    val endingOfSubtype1 = CommonDeclensions.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype
    val lastLetterOfStem = stem.takeRight(1)
    lastLetterOfStem match {
      case "к" | "г" | "х" => endingOfSubtype1.replace("ы", "и")
      case _ => endingOfSubtype1
    }
  }

  def endingOfType4(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 1
    val endingOfSubtype1 = CommonDeclensions.ending(declensionParameters)
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

  def endingOfType5(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 1
    val endingOfType1 = CommonDeclensions.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype
    if (stem.endsWith("ц") && !isEndingStressed)
      endingOfType1.replace("о", "е")
    else endingOfType1
  }

  def endingOfType6(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 2
    val endingOfSubtype2 = CommonDeclensions.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype

    val lastLetterOfStem = stem.takeRight(1)
    val ending = lastLetterOfStem match {
      case "а" | "е" | "ё" | "о" | "у" | "ы" | "э" | "ю" | "я" | "и" | "й" => endingOfSubtype2.replace("ь", "й")
      case _ => endingOfSubtype2
    }

    (declensionType, rCase, number) match {
      case (DeclensionType.Substantive, Case.Genetive, Number.Plural) =>
        gender match {
          case Gender.Masculine => if (isEndingStressed) "ёв" else "ев" //todo: добавить проверку окончания на равенство "ей"?
          case Gender.Feminine | Gender.Neuter => "й" //todo: добавить проверку окончания на равенство "ь", "ей"?
          case _ => ???
        }
      case _ => ending
    }
  }

  def endingOfType7(declensionParameters: HasStem with HasDeclensionTypeAndSubtype with HasGender with HasNumber with HasCase with HasAnimacy with HasStress) = {
    import declensionParameters._

    val oldDeclenstionSubtype = declensionParameters.declensionSubtype
    declensionParameters.declensionSubtype = 2
    val endingOfSubtype2 = CommonDeclensions.ending(declensionParameters)
    declensionParameters.declensionSubtype = oldDeclenstionSubtype

    val lastLetterOfStem = stem.takeRight(1)
    val ending = lastLetterOfStem match {
      case "и" => endingOfSubtype2.replace("ь", "й")
      case _ => endingOfSubtype2
    }

    declensionType match {
      case DeclensionType.Substantive => (rCase, number, gender) match {
        case (Case.Genetive, Number.Plural, Gender.Masculine) => if (isEndingStressed) "ёв" else "ев" //todo: добавить проверку окончания на равенство "ей"?
        case (Case.Genetive, Number.Plural, Gender.Feminine) | (Case.Genetive, Number.Plural, Gender.Neuter) => "й" //todo: добавить проверку окончания на равенство "ь", "ей"?
        case (Case.Dative, Number.Singular, Gender.Feminine) | (Case.Prepositional, Number.Singular, _) => if (isEndingStressed) "е" else "и" //todo: добавить проверку окончания на равенство "е"?
        case _ => ending
      }
    }
  }
}
