package org.zubtsov.dictionary.zaliznyak.declension

import org.zubtsov.dictionary.zaliznyak.Utils.RussianWord._
import org.zubtsov.dictionary.zaliznyak.Utils.{RussianLetter, RussianWord, _}
import org.zubtsov.dictionary.zaliznyak.attributes.common.{HasGender, HasNumber, HasStem}
import org.zubtsov.dictionary.zaliznyak.attributes.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasInitialForm, HasStress, HasSyntacticAndMorphologicalCharacteristics}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.common.{Gender, Number}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.{Case, DeclensionType}

//todo: try to remove dependency on at least primarySyntacticCharacteristic and (if possible) primaryMorphologicalCharacteristic
//aka Беглая гласная
object VolatileVowel {
  def stemWithVolatileEnding(declensionParameters: HasDeclensionTypeAndSubtype with HasStem with HasGender with HasNumber with HasCase with HasAnimacy with HasStress
    with HasSyntacticAndMorphologicalCharacteristics with HasInitialForm, ending: String) = {
    import declensionParameters._
    //todo: добавить проверки со стр. 29 Пункт А
    val potentialInflectedForm = stem + ending

    val newStem = if (VolatileVowel.isVolatileVowelInInitialForm(declensionParameters)
      && (potentialInflectedForm != initialForm) && !(rCase == Case.Instrumental && ending == "ью")) {
      stem match {
        case LastVowelRegex(_, "о", rest) => stem.replaceLastVowel("")
        case LastVowelRegex(_, "и", rest) => stem.replaceLastVowel("ь")
        case LastVowelRegex(previousLetters, lastConsonant, followingLetters)
          if lastConsonant.matches("(е|ё)") => {

          val previousLetter = previousLetters.takeRight(1)
          if (previousLetter.isVowel())
            stem.replaceLastVowel("й")
          else if (primaryMorphologicalCharacteristic.matches("(м|мо)"))
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
        case _ => ???
      }
    } else if (noVolatileVowelInInitialForm(declensionParameters)
      && volatileVowelIsInInflectedForm(declensionParameters)) {
      stem match {
        case LastConsonantRegex(perviousLetters, lastConsonant, followingLetters) => primaryMorphologicalCharacteristic match {
          case "ж" | "жо" | "с" | "со" if declensionSubtype == 6 =>
            stem.replaceLastLetter("ь", if (isEndingStressed) "ё" else "е")
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

  def isVolatileVowelInInitialForm(declensionParameters: HasDeclensionTypeAndSubtype with HasGender with HasSyntacticAndMorphologicalCharacteristics) = {
    import declensionParameters._

    (declensionType == DeclensionType.Substantive &&
      primarySyntacticCharacteristic.matches("(м|мо)") &&
      primarySyntacticCharacteristic == primaryMorphologicalCharacteristic) ||
      (gender == Gender.Feminine && declensionSubtype == 8) ||
      declensionType == DeclensionType.Pronounative
  }

  def noVolatileVowelInInitialForm(declensionParameters: HasDeclensionTypeAndSubtype with HasGender with HasSyntacticAndMorphologicalCharacteristics) = {
    import declensionParameters._

    (declensionType == DeclensionType.Substantive &&
      gender == Gender.Feminine && declensionSubtype != 8) || gender == Gender.Neuter
  }

  def volatileVowelIsInInflectedForm(declensionParameters: HasDeclensionTypeAndSubtype with HasCase with HasNumber with HasSyntacticAndMorphologicalCharacteristics) = {
    import declensionParameters._

    ((primaryMorphologicalCharacteristic.among("ж", "жо")
      && declensionSubtype != 8) ||
      primaryMorphologicalCharacteristic.among("с", "со")) &&
      number == Number.Plural &&
      (rCase == Case.Genetive || rCase == Case.Accusative) //todo: в винительном только если ending такое же как в родительном
  }
}
