package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.conjugation.BasicConjugatedForm
import nlp.dictionary.zaliznyak.feature.common.IsPartOfSpeech
import nlp.dictionary.zaliznyak.feature.conjugation.HasConjugationType
import nlp.dictionary.zaliznyak.feature.declension.HasInitialForm
import nlp.dictionary.zaliznyak.feature.enums.common.Number
import nlp.dictionary.zaliznyak.feature.enums.common.Number.Number
import nlp.dictionary.zaliznyak.feature.enums.conjugation.Person.Person
import nlp.dictionary.zaliznyak.feature.enums.conjugation.Tense.Tense
import nlp.dictionary.zaliznyak.feature.enums.conjugation.{Aspect, Person, Tense, Transitivity}
import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech

class Verb extends HasInitialForm with HasConjugationType with IsPartOfSpeech {
  outer =>

  private var _conjugationType: Int = _
  private var _infinitive: String = _


  override def partOfSpeech: PartOfSpeech = PartOfSpeech.Verb

  override def initialForm: String = _infinitive

  override def conjugationType: Int = _conjugationType

  private[Verb] class VerbForm(p: Person, n: Number, t: Tense) extends BasicConjugatedForm
    with IsPartOfSpeech {
    override def person: Person = p

    override def initialForm: String = _infinitive

    override def tense: Tense = t

//    override def stem: String = {
//      new NameWithStem().stemOfInfinitive(infinitive) //todo: generalize
//    }

    override def number: Number = n

    override def conjugationType: Int = outer.conjugationType

    override def partOfSpeech: PartOfSpeech = outer.partOfSpeech

//    override def hasVolatileVowel: Boolean = false

    def form(): String = {
      val (stem, ending) = formOfFirstOrThirdPersonPresentSingular(None)
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
      raw"([а|в|с]/?[а|в|с]?)" +
      //      raw"[\w\W]*" +
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
      stressType
      ) => {
        val reflexive = infinitive.endsWithAnyOf("ся", "сь")
        Aspect(aspect)
        Transitivity(reflexive, Option(transitive))

        val verb = new Verb()
        verb._infinitive = infinitive
        verb._conjugationType = conjugationType.toInt
        verb.inflectedForms = Seq(
          new verb.VerbForm(Person.First, Number.Singular, Tense.Present),
          new verb.VerbForm(Person.Third, Number.Singular, Tense.Present)
        )
        verb
      }
      case _ => ???
    }
  }
}