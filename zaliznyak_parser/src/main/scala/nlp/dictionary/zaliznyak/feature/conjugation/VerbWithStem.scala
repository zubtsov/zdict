package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.conjugation.BasicConjugatedForm

trait VerbWithStem extends BasicConjugatedForm {
  private val toRemoveFromInfinitive = "(ться|тись|чься|ть|ти|чь)$"
  private val toRemoveFromFirstPersonSingularPresent = "(усь|юсь|у|ю)$"
  private val toRemoveFromThirdPersonSingularPresent = "(ется|ётся|ится|ет|ёт|ит)$"

  protected def stemOfInfinitive() = {
    initialForm.replaceAll(toRemoveFromInfinitive, "")
  }
  //tense depends on aspect
  protected def stemOfFirstPersonSingularPresentOrFutureTense() = {
    val (stem, ending) = formOfFirstPersonPresentOrFutureSingularForm()
    (stem + ending)
      .replaceAll(toRemoveFromFirstPersonSingularPresent, "") //todo: just take stem???
  }
  //tense depends on aspect
  protected def stemOfThirdPersonSingularPresentOrFutureTense() = {
    val (stem, ending) = formOfThirdPersonPresentOrFutureSingular()
    (stem + ending)
      .replaceAll(toRemoveFromThirdPersonSingularPresent, "") //todo: just take stem???
  }
}
