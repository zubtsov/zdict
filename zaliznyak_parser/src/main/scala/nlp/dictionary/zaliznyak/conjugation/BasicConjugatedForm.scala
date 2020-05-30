package nlp.dictionary.zaliznyak.conjugation

import nlp.dictionary.zaliznyak.feature.common.{HasNumber, HasStem}
import nlp.dictionary.zaliznyak.feature.conjugation.{HasConjugationType, HasPerson, HasTense}
import nlp.dictionary.zaliznyak.feature.declension.HasInitialForm
import nlp.dictionary.zaliznyak.feature.enums.common.Number
import nlp.dictionary.zaliznyak.feature.enums.conjugation.{Person, Tense}

//aka Спряжение
trait BasicConjugatedForm extends HasInitialForm with HasStem with HasConjugationType with HasTense with HasPerson with HasNumber {
  private val ConsonantRotation = Map(
    "ск" -> "щ",
    "ст" -> "щ",
    "к" -> "ч",
    "т" -> "ч", //sometimes it's "щ"
    "б" -> "бл",
    "п" -> "пл",
    "в" -> "вл",
    "ф" -> "фл",
    "м" -> "мл",
    "з" -> "ж",
    "с" -> "ш",
    "д" -> "ж",
    "г" -> "ж",
    "х" -> "ш",
  )

  def endingOfFirstOrThirdPersonPresentSingular(endingHint: Option[String] = None): (String, String) = {
    import nlp.dictionary.zaliznyak.helper.Utils._

    val newStem = conjugationType match {
      case 4 | 5 if (
        person == Person.First
          && number == Number.Singular
          && (tense == Tense.Present || tense == Tense.Future)
        ) => {
        applyConsonantRotation(initialForm, endingHint)
      }
      case 6 if (
        (person == Person.First || person == Person.Third)
          && number == Number.Singular
          && (tense == Tense.Present || tense == Tense.Future)
        ) => {
        applyConsonantRotation(initialForm, endingHint)
      }
      case _ => stem
    }

    val ending = tense match {
      case Tense.Present | Tense.Future => {
        conjugationType match {
          case 1 => {
            initialForm.takeRight(3) match {
              case "ать" => firstThirdPersonEnding("аю", "ает")
              case "ять" => firstThirdPersonEnding("яю", "яет")
              case "еть" => firstThirdPersonEnding("ею", "еет")
              case _ => ???
            }
          }
          case 2 => {
            initialForm.takeRight(5) match {
              case "овать" => firstThirdPersonEnding("ую", "ует")
              case "евать" => {
                val previousLetter = initialForm.takeRight(5).take(1)
                if (previousLetter.isFizzingConsonant() || previousLetter == "ц")
                  firstThirdPersonEnding("ую", "ует")
                else
                  firstThirdPersonEnding("юю", "юет")
              }
              case _ => ???
            }
          }
          case 3 => {
            initialForm.takeRight(4) match {
              case "нуть" => firstThirdPersonEnding("ну", "нет")
              case _ => ???
            }
          }
          case 4 => {
            initialForm.takeRight(3) match {
              case "ить" => person match {
                case Person.First => if (initialForm.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case Person.Third => "ит"
                case _ => ???
              }
              case _ => ???
            }
          }
          case 5 => {
            initialForm.takeRight(3) match {
              case "ать" | "ять" | "еть" => person match {
                case Person.First => if (initialForm.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case Person.Third => "ит"
                case _ => ???
              }
              case _ => ???
            }
          }
          case 6 => {
            initialForm.takeRight(3) match {
              case "ать" | "ять" => person match {
                case Person.First => if (initialForm.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case Person.Third => "ет"
                case _ => ???
              }
              case _ => ???
            }
          }
          case 7 => {
            initialForm.takeRight(3) match {
              case "зти" | "зть" => firstThirdPersonEnding("зу", "зет")
              case "сти" | "сть" => {
                val (first, third) = Map(
                  "-c-" -> ("су", "сет"),
                  "-д-" -> ("ду", "дет"),
                  "-т-" -> ("ту", "тет"),
                  "-ст-" -> ("сту", "стет"),
                  "-б-" -> ("бу", "бсет")
                )(endingHint.get)
                firstThirdPersonEnding(first, third)
              }
              case _ => ???
            }
          }
          case 8 => {
            initialForm.takeRight(2) match {
              case "чь" => {
                val (first, third) = Map(
                  "-г-" -> ("гу", "жет"),
                  "-к-" -> ("ку", "чет")
                )(endingHint.get)
                firstThirdPersonEnding(first, third)
              }
              case _ => ???
            }
          }
          case 9 => {
            initialForm.takeRight(5) match {
              case "ереть" => firstThirdPersonEnding("ру", "рет")
              case _ => ???
            }
          }
          case 10 => {
            initialForm.takeRight(5) match {
              case "олоть" => firstThirdPersonEnding("олю", "олет")
              case "ороть" => firstThirdPersonEnding("орю", "орет")
              case _ => ???
            }
          }
          case 11 => {
            initialForm.takeRight(3) match {
              case "ить" => firstThirdPersonEnding("ью", "ьет")
              case _ => ???
            }
          }
          case 12 => { //todo: implement additional guidelines
            initialForm.takeRight(3) match {
              case "ыть" => firstThirdPersonEnding("ою", "оет")
              case "уть" => firstThirdPersonEnding("ую", "ует")
              case "ить" => firstThirdPersonEnding("ию", "иет")
              case _ => ???
            }
          }
          case 13 => {
            initialForm.takeRight(5) match {
              case "авать" => firstThirdPersonEnding("аю", "ает")
              case _ => ???
            }
          }
          case 14 => { //todo: implement additional guidelines
            initialForm.takeRight(3) match {
              case "ать" | "ять" => {
                val (first, third) = Map(
                  "-н-" -> ("ну", "нет"),
                  "-м-" -> ("му", "мет"),
                  "-им-" -> ("иму", "имет")
                )(endingHint.get)
                firstThirdPersonEnding(first, third)
              }
              case _ => ???
            }
          }
          case 15 => {
            initialForm.takeRight(2) match {
              case "ть" => firstThirdPersonEnding("ну", "нет")
              case _ => ???
            }
          }
          case 16 => {
            initialForm.takeRight(2) match {
              case "ть" => firstThirdPersonEnding("ву", "вет")
              case _ => ???
            }
          }
          case _ => ???
        }
      }
      case _ => ???
    }
    (newStem, ending)
  }

  private def firstThirdPersonEnding(first: String, third: String) = {
    person match {
      case Person.First => first
      case Person.Third => third
      case _ => ???
    }
  }

  private def applyConsonantRotation(infinitive: String, endingHint: Option[String]) = {
    import nlp.dictionary.zaliznyak.helper.Utils._

    val truncated = infinitive.dropRight(3)
    val ending = infinitive.takeRight(3)

    val replaced = if (!truncated.endsWithAnyOf(ConsonantRotation.keys))
      truncated
    else if (endingHint.forall(_ == "-щ-")
      && !truncated.endsWith("ст")
      && truncated.endsWith("т")) {
      truncated.dropRight(1) + "щ"
    } else {
      //use first replacement from the map
      val replacement = ConsonantRotation.dropWhile(t => !truncated.endsWith(t._1)).head
      truncated.replaceLastOccurence(replacement._1, replacement._2)
    }

    replaced + ending
  }
}
