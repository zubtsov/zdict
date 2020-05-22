package org.zubtsov.dictionary.zaliznyak.conjugation

import org.zubtsov.dictionary.zaliznyak.attributes.conjugation.{HasConjugationType, HasPerson, HasTense}
import org.zubtsov.dictionary.zaliznyak.attributes.declension.HasInitialForm
import org.zubtsov.dictionary.zaliznyak.attributes.enums.conjugation.{Person, Tense}

object Conjugation {
  def firstThirdPersonEnding(hasPerson: HasPerson, first: String, third: String) = {
    hasPerson.person match {
      case Person.First => first
      case Person.Third => third
      case _ => ???
    }
  }

  def basicEnding(conjugationParameters: HasInitialForm with HasConjugationType with HasTense with HasPerson) = {
    import conjugationParameters._
    import org.zubtsov.dictionary.zaliznyak.Utils._
    tense match {
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
          case 4 => { //todo: implement additional guidelines
            initialForm.takeRight(3) match {
              case "ить" => person match {
                case Person.First => if (initialForm.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case Person.Third => "ит"
                case _ => ???
              }
              case _ => ???
            }
          }
          case 5 => { //todo: implement additional guidelines
            initialForm.takeRight(3) match {
              case "ать" | "ять" | "еть" => person match {
                case Person.First => if (initialForm.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case Person.Third => "ит"
                case _ => ???
              }
              case _ => ???
            }
          }
          case 6 => { //todo: implement additional guidelines
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
  }
}
