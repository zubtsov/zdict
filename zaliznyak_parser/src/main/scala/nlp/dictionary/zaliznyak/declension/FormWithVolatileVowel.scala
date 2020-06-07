package nlp.dictionary.zaliznyak.declension

import nlp.dictionary.zaliznyak.feature.common.HasNumber
import nlp.dictionary.zaliznyak.feature.declension._
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.{Case, DeclensionType}
import nlp.dictionary.zaliznyak.helper.Utils.RussianWord._
import nlp.dictionary.zaliznyak.helper.Utils.{RussianLetter, RussianWord, _}

//todo: try to remove dependency on at least primarySyntacticCharacteristic and (if possible) primaryMorphologicalCharacteristic
//aka Беглая гласная
trait FormWithVolatileVowel extends HasDeclensionTypeAndSubtype with NameWithStem with HasGender with HasNumber with HasCase with HasAnimacy with NameWithStress
  with HasSyntacticAndMorphologicalCharacteristics with HasInitialForm {

  def hasVolatileVowel: Boolean

  protected def applyVolatileVowelRule(ending: String): String = {
    //todo: добавить проверки со стр. 29 Пункт А
    val potentialInflectedForm = stem + ending

    val newStem: String = if (isVolatileVowelInInitialForm()
      && (potentialInflectedForm != initialForm) && !(rCase == Case.Instrumental && ending == "ью")) {
      stem match {
        case LastVowelRegex(_, "о", rest) => stem.replaceLastVowel("")
        case LastVowelRegex(_, "и", rest) => stem.replaceLastVowel("ь")
        case LastVowelRegex(previousLetters, lastConsonant, followingLetters)
          if lastConsonant.matches("(е|ё)") => {

          val previousLetter = previousLetters.takeRight(1)
          if (previousLetter.isVowel())
            stem.replaceLastVowel("й")
          else if (primaryMorphologicalCharacteristic.matches("(м|мо)")) {
            if (declensionSubtype == 6 ||
              (declensionSubtype == 3
                && previousLetter.isConsonant()
                && previousLetter.matches("[^ЖШЧЩЦжшчщц]")) ||
              previousLetter == "л"
            )
              stem.replaceLastVowel("ь")
            else
              stem.replaceLastVowel("")
          }
          else
            ???
        }
        case _ => ???
      }
    } else if (noVolatileVowelInInitialForm()
      && volatileVowelIsInInflectedForm()) {
      stem match {
        case LastConsonantRegex(perviousLetters, lastConsonant, followingLetters) => primaryMorphologicalCharacteristic match {
          case "ж" | "жо" | "с" | "со" if declensionSubtype == 6 =>
            stem.replaceLastOccurence("ь", if (isEndingStressed) "ё" else "е")
          case _ if (perviousLetters.endsWithAnyOf("ь", "й")) =>
            stem.patch(perviousLetters.length - 1, if (!isEndingStressed || perviousLetters.endsWith("ц")) "е" else "ё", 1)
          case _ => {
            if (perviousLetters.endsWithAnyOf("к", "г", "х"))
              perviousLetters + "о" + lastConsonant + followingLetters
            else if (!perviousLetters.endsWithAnyOf("ж", "ш", "ч", "щ", "ц") && lastConsonant.among("к", "г", "х")) //declensionSubtype == 3?
              perviousLetters + "о" + lastConsonant + followingLetters
            else if (!isEndingStressed || lastConsonant == "ц")
              perviousLetters + "е" + lastConsonant + followingLetters
            else if (isEndingStressed && perviousLetters.takeRight(1).isFizzingConsonant())
              perviousLetters + "о" + lastConsonant + followingLetters
            else
              perviousLetters + "ё" + lastConsonant + followingLetters
          }
        }
        case _ => ???
      }
    } else {
      stem
    }

    newStem
  }

  private def isVolatileVowelInInitialForm() = {
    (declensionType == DeclensionType.Substantive &&
      primarySyntacticCharacteristic.matches("(м|мо)") &&
      primarySyntacticCharacteristic == primaryMorphologicalCharacteristic) ||
      (gender == Gender.Feminine && declensionSubtype == 8) ||
      declensionType == DeclensionType.Pronounative
  }

  private def noVolatileVowelInInitialForm() = {
    (declensionType == DeclensionType.Substantive &&
      gender == Gender.Feminine && declensionSubtype != 8) || gender == Gender.Neuter
  }

  private def volatileVowelIsInInflectedForm() = {
    ((primaryMorphologicalCharacteristic.among("ж", "жо")
      && declensionSubtype != 8) ||
      primaryMorphologicalCharacteristic.among("с", "со")) &&
      number == Number.Plural &&
      (rCase == Case.Genetive || rCase == Case.Accusative) //todo: в винительном только если ending такое же как в родительном
  }
}
