package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.conjugation.Conjugation
import nlp.dictionary.zaliznyak.feature.common.{HasNumber, HasStem, IsPartOfSpeech}
import nlp.dictionary.zaliznyak.feature.conjugation.{HasConjugationType, HasPerson, HasTense}
import nlp.dictionary.zaliznyak.feature.declension.HasInitialForm
import nlp.dictionary.zaliznyak.feature.enums.conjugation.Person.Person
import nlp.dictionary.zaliznyak.feature.enums.conjugation.Tense.Tense
import nlp.dictionary.zaliznyak.feature.enums.conjugation.{Aspect, Transitivity}
import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech

class Verb extends HasInitialForm with HasConjugationType with IsPartOfSpeech {
  outer =>

  private var _conjugationType: Int = _
  private var _infinitive: String = _


  override def partOfSpeech: PartOfSpeech = PartOfSpeech.Verb

  override def initialForm: String = _infinitive

  override def conjugationType: Int = _conjugationType

  private[Verb] trait VerbSpecificAttributes extends HasInitialForm with HasConjugationType with IsPartOfSpeech

  private[Verb] trait VerbFormSpecificAttributes extends HasStem with HasTense with HasPerson with HasNumber

  private[Verb] class VerbForm(p: Person, iForm: String, t: Tense, s: String) extends VerbSpecificAttributes with VerbFormSpecificAttributes {
    private val _person: Person = p
    private val _initialForm: String = iForm
    private val _tense: Tense = t
    private val _stem: String = s


    override def conjugationType: Int = outer.conjugationType

    override def partOfSpeech: PartOfSpeech = outer.partOfSpeech

    override def hasVolatileVowel: Boolean = false

    override def person: Person = _person

    override def initialForm: String = _initialForm

    override def tense: Tense = _tense

    override def stem: String = _stem

    private val _form: String = stem + new Conjugation().ending(this, None)

    def form(): String = _form
  }

  private var inflectedForms: Seq[VerbForm] = _
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
      conjugationIndex,
      volatileVowelIndicator,
      stressType
      ) => {
        val reflexive = infinitive.endsWithAnyOf("ся", "сь")
        Aspect(aspect)
        Transitivity(reflexive, Option(transitive))

        val verb = new Verb()
        verb._infinitive = infinitive

        verb
      }
      case _ => ???
    }
  }
}