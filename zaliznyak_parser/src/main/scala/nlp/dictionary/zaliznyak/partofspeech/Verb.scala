package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.conjugation.{BasicConjugatedForm, ConjugatedForm}
import nlp.dictionary.zaliznyak.feature.common.IsPartOfSpeech
import nlp.dictionary.zaliznyak.feature.conjugation.{HasAspect, HasConjugationType, HasEndingHint, HasReflection}
import nlp.dictionary.zaliznyak.feature.declension.HasInitialForm
import nlp.dictionary.zaliznyak.feature.enums.common.Gender.Gender
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.common.Number.Number
import nlp.dictionary.zaliznyak.feature.enums.conjugation.Aspect.Aspect
import nlp.dictionary.zaliznyak.feature.enums.conjugation.Mood.Mood
import nlp.dictionary.zaliznyak.feature.enums.conjugation.PastStressType.PastStressType
import nlp.dictionary.zaliznyak.feature.enums.conjugation.Person.Person
import nlp.dictionary.zaliznyak.feature.enums.conjugation.PresentStressType.PresentStressType
import nlp.dictionary.zaliznyak.feature.enums.conjugation.Tense.Tense
import nlp.dictionary.zaliznyak.feature.enums.conjugation.{Aspect, Mood, PastStressType, Person, PresentStressType, Tense, Transitivity}
import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech

class Verb private() extends HasInitialForm with HasConjugationType with HasEndingHint with HasReflection with HasAspect with IsPartOfSpeech {
  outer =>

  private var _pastStressType: PastStressType = _
  private var _presentStressType: PresentStressType = _
  private var _reflexive: Boolean = _
  private var _endingHint: Option[String] = _
  private var _conjugationType: Int = _
  private var _infinitive: String = _
  private var _aspect: Aspect = _


  override def isReflexive(): Boolean = _reflexive

  override def endingHint: Option[String] = _endingHint

  override def partOfSpeech: PartOfSpeech = PartOfSpeech.Verb

  override def initialForm: String = _infinitive

  override def conjugationType: Int = _conjugationType

  override def aspect(): Aspect = _aspect

  private[Verb] class VerbForm(p: Option[Person], n: Number, t: Tense, g: Option[Gender], m: Mood) extends ConjugatedForm
    with IsPartOfSpeech {
    if (_aspect == Aspect.Perfect && t == Tense.Present) {
      throw new Exception("Verbs of perfect aspect may only have past and simple future forms!")
    }

    override def person: Option[Person] = p

    override def initialForm: String = _infinitive

    override def tense: Tense = t

    //    override def stem: String = {
    //      new NameWithStem().stemOfInfinitive(infinitive) //todo: generalize
    //    }

    override def number: Number = n

    override def mood(): Mood = m

    override def gender: Option[Gender] = g

    override def aspect(): Aspect = _aspect

    override def conjugationType: Int = outer.conjugationType

    override def endingHint: Option[String] = outer._endingHint

    override def isReflexive(): Boolean = outer._reflexive

    override def partOfSpeech: PartOfSpeech = outer.partOfSpeech

    //    override def hasVolatileVowel: Boolean = false

    override def pastStressType: PastStressType = _pastStressType

    override def presentStressType: PresentStressType = _presentStressType

    def form(): String = {
      val (stem, ending) = verbForm()
      stem + ending
    }

    override def toString: String = form
  }

  private var inflectedForms: Seq[VerbForm] = _

  override def toString: String = partOfSpeech + ": " + inflectedForms.mkString(",")
}

object Verb {
  val regex = (
    //начальная форма глагола (инфинитив)
    raw"^([а-яА-Я\-]+)\s" +
      //позиция ударной гласной
      raw"([0-9]+\.?[0-9]?)\s" +
      //вид
      raw"(св|нсв|св-нсв)\s" +
      //переходность
      raw"(нп)?\s?" +
      //тип спряжения
      raw"([0-9])+" +
      //беглая гласная
      raw"(\*{1,2})?" +
      //схема ударения
      raw"([а|в|с]/?[а|в|с]?)\s" +
      raw"(\(-[а-я]+-\))?" + //todo: narrow it?
      raw"[\w\W]*" +
      "$"
    ).r

  def apply(dictionaryRecord: String): Verb = {
    import nlp.dictionary.zaliznyak.helper.Utils.RussianWord
    dictionaryRecord match {
      case regex(
      infinitive,
      stressedVowelsPositions,
      aspect,
      transitive,
      conjugationType,
      volatileVowelIndicator,
      stressType,
      endingHint
      ) => {
        val isReflexive = infinitive.endsWithAnyOf("ся", "сь")
        Transitivity(isReflexive, Option(transitive))

        val verb = new Verb()
        verb._aspect = Aspect(aspect)
        verb._pastStressType = PastStressType(stressType)
        verb._presentStressType = PresentStressType(stressType)
        verb._reflexive = isReflexive
        verb._infinitive = infinitive
        verb._conjugationType = conjugationType.toInt
        verb._endingHint = if (endingHint != null) Option(endingHint.dropRight(2).drop(2)) else None
        //todo: refactor, replace with a loop or lambda
        verb.inflectedForms = if (verb._aspect == Aspect.Perfect) {
          Seq(
            new verb.VerbForm(None, Number.Singular, Tense.Future, Option(Gender.Masculine), Mood.Indicative),
            new verb.VerbForm(None, Number.Singular, Tense.Future, Option(Gender.Feminine), Mood.Indicative),
            new verb.VerbForm(None, Number.Singular, Tense.Future, Option(Gender.Neuter), Mood.Indicative),
            new verb.VerbForm(None, Number.Plural, Tense.Future, Option(Gender.Masculine), Mood.Indicative),
            new verb.VerbForm(None, Number.Plural, Tense.Future, Option(Gender.Feminine), Mood.Indicative),
            new verb.VerbForm(None, Number.Plural, Tense.Future, Option(Gender.Neuter), Mood.Indicative),

            new verb.VerbForm(Option(Person.First), Number.Singular, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.First), Number.Plural, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.Second), Number.Singular, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.Second), Number.Plural, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.Third), Number.Singular, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.Third), Number.Plural, Tense.Future, None, Mood.Indicative)
          )
        } else if (verb._aspect == Aspect.Imperfect) {
          Seq(
            new verb.VerbForm(None, Number.Singular, Tense.Future, Option(Gender.Masculine), Mood.Indicative),
            new verb.VerbForm(None, Number.Singular, Tense.Future, Option(Gender.Feminine), Mood.Indicative),
            new verb.VerbForm(None, Number.Singular, Tense.Future, Option(Gender.Neuter), Mood.Indicative),
            new verb.VerbForm(None, Number.Plural, Tense.Future, Option(Gender.Masculine), Mood.Indicative),
            new verb.VerbForm(None, Number.Plural, Tense.Future, Option(Gender.Feminine), Mood.Indicative),
            new verb.VerbForm(None, Number.Plural, Tense.Future, Option(Gender.Neuter), Mood.Indicative),

            new verb.VerbForm(Option(Person.First), Number.Singular, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.First), Number.Plural, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.Second), Number.Singular, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.Second), Number.Plural, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.Third), Number.Singular, Tense.Future, None, Mood.Indicative),
            new verb.VerbForm(Option(Person.Third), Number.Plural, Tense.Future, None, Mood.Indicative)
          )
        }
        else ???
        verb
      }
      case _ => ???
    }
  }
}