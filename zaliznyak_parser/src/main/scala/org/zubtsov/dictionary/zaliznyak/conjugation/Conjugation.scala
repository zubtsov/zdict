package org.zubtsov.dictionary.zaliznyak.conjugation

import org.zubtsov.dictionary.zaliznyak.attributes.common.{HasNumber, HasStem}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.common.Number
import org.zubtsov.dictionary.zaliznyak.attributes.conjugation.{HasConjugationType, HasPerson, HasTense}
import org.zubtsov.dictionary.zaliznyak.attributes.declension.HasInitialForm
import org.zubtsov.dictionary.zaliznyak.attributes.enums.conjugation.{Person, Tense}

object Conjugation {
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

  def firstThirdPersonEnding(hasPerson: HasPerson, first: String, third: String) = {
    hasPerson.person match {
      case Person.First => first
      case Person.Third => third
      case _ => ???
    }
  }

  def applyConsonantRotation(infinitive: String, endingHint: Option[String]) = {
    import org.zubtsov.dictionary.zaliznyak.helpers.Utils._

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

  def basicConjugation(conjugationParameters: HasInitialForm with HasStem with HasConjugationType with HasTense with HasPerson with HasNumber,
                       endingHint: Option[String] = None): (String, String) = {
    import conjugationParameters._
    import org.zubtsov.dictionary.zaliznyak.helpers.Utils._

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
              case "ать" => firstThirdPersonEnding(conjugationParameters, "аю", "ает")
              case "ять" => firstThirdPersonEnding(conjugationParameters, "яю", "яет")
              case "еть" => firstThirdPersonEnding(conjugationParameters, "ею", "еет")
              case _ => ???
            }
          }
          case 2 => {
            initialForm.takeRight(4) match {
              case "овать" => firstThirdPersonEnding(conjugationParameters, "ую", "ует")
              case "евать" => {
                val previousLetter = initialForm.takeRight(5).take(1)
                if (previousLetter.isFizzingConsonant() || previousLetter == "ц")
                  firstThirdPersonEnding(conjugationParameters, "ую", "ует")
                else
                  firstThirdPersonEnding(conjugationParameters, "юю", "юет")
              }
              case _ => ???
            }
          }
          case 3 => {
            initialForm.takeRight(4) match {
              case "нуть" => firstThirdPersonEnding(conjugationParameters, "ну", "нет")
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
          case 7 => { //todo: implement additional guidelines
            initialForm.takeRight(3) match {
              case "зти" | "зть" => firstThirdPersonEnding(conjugationParameters, "зу", "зет")
              case "сти" | "сть" => ??? //todo: implement
              case _ => ???
            }
          }
          case 8 => { //todo: implement additional guidelines
            initialForm.takeRight(2) match {
              case "чь" => ??? //todo: implement
              case _ => ???
            }
          }
          case 9 => {
            initialForm.takeRight(5) match {
              case "ереть" => firstThirdPersonEnding(conjugationParameters, "ру", "рет")
              case _ => ???
            }
          }
          case 10 => {
            initialForm.takeRight(5) match {
              case "олоть" => firstThirdPersonEnding(conjugationParameters, "олю", "олет")
              case "ороть" => firstThirdPersonEnding(conjugationParameters, "орю", "орет")
              case _ => ???
            }
          }
          case 11 => {
            initialForm.takeRight(3) match {
              case "ить" => firstThirdPersonEnding(conjugationParameters, "ью", "ьет")
              case _ => ???
            }
          }
          case 12 => { //todo: implement additional guidelines
            initialForm.takeRight(3) match {
              case "ыть" => firstThirdPersonEnding(conjugationParameters, "ою", "оет")
              case "уть" => firstThirdPersonEnding(conjugationParameters, "ую", "ует")
              case "ить" => firstThirdPersonEnding(conjugationParameters, "ию", "иет")
              case _ => ???
            }
          }
          case 13 => {
            initialForm.takeRight(5) match {
              case "авать" => firstThirdPersonEnding(conjugationParameters, "аю", "ает")
              case _ => ???
            }
          }
          case 14 => { //todo: implement additional guidelines
            initialForm.takeRight(3) match {
              case "ать" | "ять" => ??? //todo: implement
              case _ => ???
            }
          }
          case 15 => {
            initialForm.takeRight(2) match {
              case "ть" => firstThirdPersonEnding(conjugationParameters, "ну", "нет")
              case _ => ???
            }
          }
          case 16 => {
            initialForm.takeRight(2) match {
              case "ть" => firstThirdPersonEnding(conjugationParameters, "ву", "вет")
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
}
