package nlp.dictionary.zaliznyak.feature.conjugation

import nlp.dictionary.zaliznyak.conjugation.BasicConjugatedForm
import nlp.dictionary.zaliznyak.feature.enums.conjugation.PrimaryConjugationType
import nlp.dictionary.zaliznyak.feature.enums.conjugation.PrimaryConjugationType.PrimaryConjugationType
import nlp.dictionary.zaliznyak.helper.Utils._

trait VerbPrimaryConjugationType extends BasicConjugatedForm {
  def primaryConjugationType(): PrimaryConjugationType = {
    val (stem, ending) = formOfFirstOrThirdPersonPresentSingular()
    if(ending.endsWithAnyOf("ет", "ется", "ёт", "ётся"))
      PrimaryConjugationType.First
    else if (ending.endsWithAnyOf("ит", "ится"))
      PrimaryConjugationType.Second
    else
      ???
  }
}
