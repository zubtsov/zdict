package nlp.dictionary.zaliznyak.conjugation

import nlp.dictionary.zaliznyak.feature.common.HasNumber
import nlp.dictionary.zaliznyak.feature.conjugation.{HasAspect, HasConjugationType, HasEndingHint, HasPerson, HasReflection, HasTense}
import nlp.dictionary.zaliznyak.feature.declension.HasInitialForm
import nlp.dictionary.zaliznyak.feature.enums.common.Number
import nlp.dictionary.zaliznyak.feature.enums.conjugation.{Aspect, Person, Tense}

//aka Спряжение //todo: with HasStem? //todo: check endingHint existence
trait BasicConjugatedForm extends HasInitialForm with HasConjugationType with HasTense with HasPerson with HasNumber with HasEndingHint with HasReflection with HasAspect {
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
  //tense depends on Aspect
  def formOfFirstPersonPresentOrFutureSingularForm(): (String, String) = {
    import nlp.dictionary.zaliznyak.helper.Utils._

    val infinitive = if (isReflexive) initialForm.dropRight(2) else initialForm //todo: there is no such step in the dictionary...

    val (initialStem, ending) = tense match {
      case Tense.Present | Tense.Future => {
        conjugationType match {
          case 1 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" => "аю"
              case "ять" => "яю"
              case "еть" => "ею"
              case _ => ???
            })
          }
          case 2 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "овать" => "ую"
              case "евать" => {
                val previousLetter = infinitive.takeRight(5).take(1)
                if (previousLetter.isFizzingConsonant() || previousLetter == "ц")
                  "ую"
                else
                  "юю"
              }
              case _ => ???
            })
          }
          case 3 => {
            (infinitive.dropRight(4), infinitive.takeRight(4) match {
              case "нуть" => "ну"
              case _ => ???
            })
          }
          case 4 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ить" => person match {
                case Person.First => if (infinitive.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 5 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" | "еть" => person match {
                case Person.First => if (infinitive.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 6 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" => person match {
                case Person.First => if (infinitive.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 7 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "зти" | "зть" => "зу"
              case "сти" | "сть" => {
                val first = Map(
                  "c" -> "су",
                  "д" -> "ду",
                  "т" -> "ту",
                  "ст" -> "сту",
                  "б" -> "бу"
                )(endingHint.get)
                first
              }
              case _ => ???
            })
          }
          case 8 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "чь" => {
                val first = Map(
                  "г" -> "гу",
                  "к" -> "ку"
                )(endingHint.get)
                first
              }
              case _ => ???
            })
          }
          case 9 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "ереть" => "ру"
              case _ => ???
            })
          }
          case 10 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "олоть" => "олю"
              case "ороть" => "орю"
              case _ => ???
            })
          }
          case 11 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ить" => "ью"
              case _ => ???
            })
          }
          case 12 => { //todo: implement additional guidelines
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ыть" => "ою"
              case "уть" => "ую"
              case "ить" => "ию"
              case _ => ???
            })
          }
          case 13 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "авать" => "аю"
              case _ => ???
            })
          }
          case 14 => { //todo: implement additional guidelines
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" => {
                val first = Map(
                  "н" -> "ну",
                  "м" -> "му",
                  "им" -> "иму"
                )(endingHint.get)
                first
              }
              case _ => ???
            })
          }
          case 15 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "ть" => "ну"
              case _ => ???
            })
          }
          case 16 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "ть" => "ву"
              case _ => ???
            })
          }
          case _ => ???
        }
      }
      case _ => ???
    }

    val newStem = conjugationType match {
      case 4 | 5 if (
        person == Person.First
          && number == Number.Singular
          && (tense == Tense.Present || tense == Tense.Future)
        ) => {
        applyConsonantRotation(initialStem)
      }
      case 6 if (
        (person == Person.First)
          && number == Number.Singular
          && (tense == Tense.Present || tense == Tense.Future)
        ) => {
        applyConsonantRotation(initialStem)
      }
      case _ => initialStem
    }

    val postfix = if (isReflexive()) {
      person match {
        case Person.First => "сь"
        case _ => ???
      }
    } else ""

    (newStem, ending + postfix)
  }
  //tense depends on Aspect
  def formOfThirdPersonPresentOrFutureSingular(): (String, String) = {
    import nlp.dictionary.zaliznyak.helper.Utils._

    val infinitive = if (isReflexive) initialForm.dropRight(2) else initialForm //todo: there is no such step in the dictionary...

    val (initialStem, ending) = tense match {
      case Tense.Present | Tense.Future => {
        conjugationType match {
          case 1 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" => "ает"
              case "ять" => "яет"
              case "еть" => "еет"
              case _ => ???
            })
          }
          case 2 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "овать" => "ует"
              case "евать" => {
                val previousLetter = infinitive.takeRight(5).take(1)
                if (previousLetter.isFizzingConsonant() || previousLetter == "ц")
                  "ует"
                else
                  "юет"
              }
              case _ => ???
            })
          }
          case 3 => {
            (infinitive.dropRight(4), infinitive.takeRight(4) match {
              case "нуть" => "нет"
              case _ => ???
            })
          }
          case 4 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ить" => person match {
                case Person.Third => "ит"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 5 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" | "еть" => person match {
                case Person.Third => "ит"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 6 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" => person match {
                case Person.Third => "ет"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 7 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "зти" | "зть" => "зет"
              case "сти" | "сть" => {
                val third = Map(
                  "c" -> "сет",
                  "д" -> "дет",
                  "т" -> "тет",
                  "ст" -> "стет",
                  "б" -> "бсет"
                )(endingHint.get)
                third
              }
              case _ => ???
            })
          }
          case 8 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "чь" => {
                val third = Map(
                  "г" -> "жет",
                  "к" -> "чет"
                )(endingHint.get)
                third
              }
              case _ => ???
            })
          }
          case 9 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "ереть" => "рет"
              case _ => ???
            })
          }
          case 10 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "олоть" => "олет"
              case "ороть" => "орет"
              case _ => ???
            })
          }
          case 11 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ить" => "ьет"
              case _ => ???
            })
          }
          case 12 => { //todo: implement additional guidelines
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ыть" => "оет"
              case "уть" => "ует"
              case "ить" => "иет"
              case _ => ???
            })
          }
          case 13 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "авать" => "ает"
              case _ => ???
            })
          }
          case 14 => { //todo: implement additional guidelines
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" => {
                val third = Map(
                  "н" -> "нет",
                  "м" -> "мет",
                  "им" -> "имет"
                )(endingHint.get)
                third
              }
              case _ => ???
            })
          }
          case 15 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "ть" => "нет"
              case _ => ???
            })
          }
          case 16 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "ть" => "вет"
              case _ => ???
            })
          }
          case _ => ???
        }
      }
      case _ => ???
    }

    val newStem = conjugationType match {
      case 6 if (
        person == Person.Third
          && number == Number.Singular
          && (tense == Tense.Present || tense == Tense.Future)
        ) => {
        applyConsonantRotation(initialStem)
      }
      case _ => initialStem
    }

    val postfix = if (isReflexive()) {
      person match {
        case Person.Third => "ся"
        case _ => ???
      }
    } else ""

    (newStem, ending + postfix)
  }

  def formOfFirstOrThirdPersonPresentSingular(): (String, String) = {
    person match {
      case Person.First => formOfFirstPersonPresentOrFutureSingularForm()
      case Person.Third => formOfThirdPersonPresentOrFutureSingular()
      case _ => ???
    }
  }

  @deprecated("todo: remove")
  def formOfFirstOrThirdPersonPresentSingularOriginal(): (String, String) = {
    import nlp.dictionary.zaliznyak.helper.Utils._

    val infinitive = if (isReflexive) initialForm.dropRight(2) else initialForm //todo: there is no such step in the dictionary...

    val (initialStem, ending) = tense match {
      case Tense.Present | Tense.Future => {
        conjugationType match {
          case 1 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" => firstThirdPersonEnding("аю", "ает")
              case "ять" => firstThirdPersonEnding("яю", "яет")
              case "еть" => firstThirdPersonEnding("ею", "еет")
              case _ => ???
            })
          }
          case 2 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "овать" => firstThirdPersonEnding("ую", "ует")
              case "евать" => {
                val previousLetter = infinitive.takeRight(5).take(1)
                if (previousLetter.isFizzingConsonant() || previousLetter == "ц")
                  firstThirdPersonEnding("ую", "ует")
                else
                  firstThirdPersonEnding("юю", "юет")
              }
              case _ => ???
            })
          }
          case 3 => {
            (infinitive.dropRight(4), infinitive.takeRight(4) match {
              case "нуть" => firstThirdPersonEnding("ну", "нет")
              case _ => ???
            })
          }
          case 4 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ить" => person match {
                case Person.First => if (infinitive.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case Person.Third => "ит"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 5 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" | "еть" => person match {
                case Person.First => if (infinitive.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case Person.Third => "ит"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 6 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" => person match {
                case Person.First => if (infinitive.dropRight(3).endsWithFizzingConsonant()) "у" else "ю"
                case Person.Third => "ет"
                case _ => ???
              }
              case _ => ???
            })
          }
          case 7 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "зти" | "зть" => firstThirdPersonEnding("зу", "зет")
              case "сти" | "сть" => {
                val (first, third) = Map(
                  "c" -> ("су", "сет"),
                  "д" -> ("ду", "дет"),
                  "т" -> ("ту", "тет"),
                  "ст" -> ("сту", "стет"),
                  "б" -> ("бу", "бсет")
                )(endingHint.get)
                firstThirdPersonEnding(first, third)
              }
              case _ => ???
            })
          }
          case 8 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "чь" => {
                val (first, third) = Map(
                  "г" -> ("гу", "жет"),
                  "к" -> ("ку", "чет")
                )(endingHint.get)
                firstThirdPersonEnding(first, third)
              }
              case _ => ???
            })
          }
          case 9 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "ереть" => firstThirdPersonEnding("ру", "рет")
              case _ => ???
            })
          }
          case 10 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "олоть" => firstThirdPersonEnding("олю", "олет")
              case "ороть" => firstThirdPersonEnding("орю", "орет")
              case _ => ???
            })
          }
          case 11 => {
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ить" => firstThirdPersonEnding("ью", "ьет")
              case _ => ???
            })
          }
          case 12 => { //todo: implement additional guidelines
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ыть" => firstThirdPersonEnding("ою", "оет")
              case "уть" => firstThirdPersonEnding("ую", "ует")
              case "ить" => firstThirdPersonEnding("ию", "иет")
              case _ => ???
            })
          }
          case 13 => {
            (infinitive.dropRight(5), infinitive.takeRight(5) match {
              case "авать" => firstThirdPersonEnding("аю", "ает")
              case _ => ???
            })
          }
          case 14 => { //todo: implement additional guidelines
            (infinitive.dropRight(3), infinitive.takeRight(3) match {
              case "ать" | "ять" => {
                val (first, third) = Map(
                  "н" -> ("ну", "нет"),
                  "м" -> ("му", "мет"),
                  "им" -> ("иму", "имет")
                )(endingHint.get)
                firstThirdPersonEnding(first, third)
              }
              case _ => ???
            })
          }
          case 15 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "ть" => firstThirdPersonEnding("ну", "нет")
              case _ => ???
            })
          }
          case 16 => {
            (infinitive.dropRight(2), infinitive.takeRight(2) match {
              case "ть" => firstThirdPersonEnding("ву", "вет")
              case _ => ???
            })
          }
          case _ => ???
        }
      }
      case _ => ???
    }

    val newStem = conjugationType match {
      case 4 | 5 if (
        person == Person.First
          && number == Number.Singular
          && (tense == Tense.Present || tense == Tense.Future)
        ) => {
        applyConsonantRotation(initialStem)
      }
      case 6 if (
        (person == Person.First || person == Person.Third)
          && number == Number.Singular
          && (tense == Tense.Present || tense == Tense.Future)
        ) => {
        applyConsonantRotation(initialStem)
      }
      case _ => initialStem
    }

    val postfix = if (isReflexive()) {
      person match {
        case Person.First => "сь"
        case Person.Third => "ся"
        case _ => ???
      }
    } else ""

    (newStem, ending + postfix)
  }

  private def firstThirdPersonEnding(first: String, third: String) = {
    person match {
      case Person.First => first
      case Person.Third => third
      case _ => ???
    }
  }

  private def applyConsonantRotation(stem: String) = {
    import nlp.dictionary.zaliznyak.helper.Utils._

    val replaced = if (!stem.endsWithAnyOf(ConsonantRotation.keys))
      stem
    else if (endingHint.forall(_ == "щ")
      && !stem.endsWith("ст")
      && stem.endsWith("т")) {
      stem.dropRight(1) + "щ"
    } else {
      //use first replacement from the map
      val replacement = ConsonantRotation.dropWhile(t => !stem.endsWith(t._1)).head
      stem.replaceLastOccurence(replacement._1, replacement._2)
    }

    replaced
  }
}
