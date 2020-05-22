package org.zubtsov.dictionary.zaliznyak

object Utils {


  def firstNotNull(args: String*) = {
    args.dropWhile(_ == null).head
  }

  def determinePartOfSpeech(declensionIndicator: String): String = declensionIndicator match {
    case "м" | "ж" | "с" | "мо" | "жо" | "со" | "мо-жо" => "сущ."
    case "св" | "нсв" | "св-нсв" => "глаг."
    case "п" => "прил."
    case _ => ???
  }

  implicit class RichString(string: String) {
    def among(letters: String*) = {
      letters.toSet.contains(string)
    }
  }

  implicit class RussianLetter(letter: String) {
    assert(letter.length == 1)

    def isVowel() = {
      letter.matches("[АЕЁИОУЫЭЮЯаеёиоуыэюя]")
    }

    def isConsonant() = {
      letter.matches("[БВГДЖЗЙКЛМНПРСТФХЦЧШЩбвгджзйклмнпрстфхцчшщ]")
    }
    //todo: fizzing or hissing?
    //aka Шипящая согласная
    def isFizzingConsonant() = {
      letter.matches("[ЖШЧЩжшчщ]")
    }
  }

  implicit class RussianWord(word: String) {
    def endsWithFizzingConsonant() = {
      word.takeRight(1).isFizzingConsonant()
    }

    def endsWithAnyOf(endings: String*): Boolean = {
      endings.exists(word.endsWith(_))
    }

    def replaceLastVowel(replacement: String) = {
      word.replaceAll(RussianWord.LastVowelOnlyExpr, replacement)
    }

    def replaceLastLetter(character: String, replacement: String) = {
      word.patch(word.lastIndexOf(character), Seq(character), 1)
    }

    def lastConsonant() = {
      word match {
        case RussianWord.LastConsonantRegex(_, lastConsonant, _) => lastConsonant
        case _ => ???
      }
    }

    def secondToLastConsonant() = {
      word match {
        case RussianWord.LastConsonantRegex(beginning, _, _) => beginning match {
          case RussianWord.LastConsonantRegex(_, secondToLastConsonant, _) => secondToLastConsonant
          case _ => ???
        }
        case _ => ???
      }
    }
  }

  object RussianWord {
    val LastConsonantExpr = raw"([А-Яа-я]*)([БВГДЖЗЙКЛМНПРСТФХЦЧШЩбвгджзйклмнпрстфхцчшщ])(?=([АЕЁИОУЫЭЮЯаеёиоуыэюя]*$$))[А-Яа-я]*$$"
    val LastConsonantRegex = LastConsonantExpr.r

    val LastVowelExpr = raw"([А-Яа-я]*)([АЕЁИОУЫЭЮЯаеёиоуыэюя])(?=([БВГДЖЗЙКЛМНПРСТФХЦЧШЩбвгджзйклмнпрстфхцчшщ]*$$))[А-Яа-я]*$$"
    val LastVowelRegex = LastVowelExpr.r

    val LastVowelOnlyExpr = raw"([АЕЁИОУЫЭЮЯаеёиоуыэюя])(?=([БВГДЖЗЙКЛМНПРСТФХЦЧШЩбвгджзйклмнпрстфхцчшщ]*$$))"
  }

}
