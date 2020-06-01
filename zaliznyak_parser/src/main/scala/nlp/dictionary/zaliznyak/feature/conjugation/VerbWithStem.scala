package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.conjugation.BasicConjugatedForm

trait VerbWithStem extends BasicConjugatedForm {
  private val toRemoveFromInfinitive = "ться|тись|чься|ть|ти|чь$"
  private val toRemoveFromFirstPersonSingularPresent = "усь|юсь|у|ю$"
  private val toRemoveFromThirdPersonSingularPresent = "ется|ётся|ится|ет|ёт|ит$"

  //todo: add reflexive parameter?
  // maybe it's better to use a separate interface to get rid of parameters?
  protected def stemOfInfinitive(infinitive: String) = {
    initialForm.replaceAll(toRemoveFromInfinitive, "")
  }

  protected def stemOfFirstPersonSingularPresentTense() = {
    val (stem, ending) = formOfFirstOrThirdPersonPresentSingular(None)
    (stem + ending)
      .replaceAll(toRemoveFromFirstPersonSingularPresent, "") //todo: just take stem???
  }

  protected def stemOfThirdPersonSingularPresentTense() = {
    val (stem, ending) = formOfFirstOrThirdPersonPresentSingular(None)
    (stem + ending)
      .replaceAll(toRemoveFromThirdPersonSingularPresent, "") //todo: just take stem???
  }
}
