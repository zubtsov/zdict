package nlp.dictionary.zaliznyak.feature.common

trait VerbWithStem {
  private val toRemoveFromInfinitive = "ться|тись|чься|ть|ти|чь$"
  private val toRemoveFromFirstPersonSingularPresent = "усь|юсь|у|ю$"
  private val toRemoveFromThirdPersonSingularPresent = "ется|ётся|ится|ет|ёт|ит$"

  //todo: add reflexive parameter?
  // maybe it's better to use a separate interface to get rid of parameters?
  protected def stemOfInfinitive(infinitive: String) = {
    infinitive.replaceAll(toRemoveFromInfinitive, "")
  }

  protected def stemOfFirstPersonSingularPresentTense(firstPersonSingularPresentForm: String) = {
    firstPersonSingularPresentForm.replaceAll(toRemoveFromFirstPersonSingularPresent, "")
  }

  protected def stemOfThirdPersonSingularPresentTense(thirdPersonSingularPresentForm: String) = {
    thirdPersonSingularPresentForm.replaceAll(toRemoveFromThirdPersonSingularPresent, "")
  }
}
