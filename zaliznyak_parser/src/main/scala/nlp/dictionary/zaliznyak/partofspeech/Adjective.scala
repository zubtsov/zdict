package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.Stemmer
import nlp.dictionary.zaliznyak.declension.InflectedForm
import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber, HasStem, IsPartOfSpeech}
import nlp.dictionary.zaliznyak.feature.declension._
import nlp.dictionary.zaliznyak.feature.enums.common.Gender.Gender
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.Animacy.Animacy
import nlp.dictionary.zaliznyak.feature.enums.declension.Case.Case
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType.DeclensionType
import nlp.dictionary.zaliznyak.feature.enums.declension.PrimaryStressType.PrimaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension.SecondaryStressType.SecondaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension._
import nlp.dictionary.zaliznyak.helper.Utils
import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech

class Adjective extends CommonName {
  outer =>

  override def partOfSpeech: PartOfSpeech = PartOfSpeech.Adjective

  private[Adjective] trait AdjectiveSpecificAttributes extends HasDeclensionTypeAndSubtype with HasStressType with HasStem with IsPartOfSpeech
    with HasSyntacticAndMorphologicalCharacteristics with HasInitialForm

  private[Adjective] trait AdjectiveFormSpecificAttributes extends HasGender with HasNumber with HasCase with HasAnimacy with HasStress

  // todo: why can't we define type as mixin of multiple traits and inherit it?
  class AdjectiveForm private[Adjective]() extends AdjectiveSpecificAttributes
    with AdjectiveFormSpecificAttributes {
    private[Adjective] var _gender: Gender = _
    private[Adjective] var _animacy: Animacy = _

    override def gender: Gender = _gender

    override def animacy: Animacy = _animacy

    override def stem: String = outer.stem

    override def partOfSpeech: PartOfSpeech = outer.partOfSpeech

    override def declensionType: DeclensionType = outer.declensionType

    override def declensionSubtype: Int = outer.declensionSubtype

    override def declensionSubtype_=(declensionSubtype: Int): Unit = outer.declensionSubtype_=(declensionSubtype)

    override def primaryStressType: PrimaryStressType = outer.primaryStressType

    override def secondaryStressType: Option[SecondaryStressType] = outer.secondaryStressType

    override def hasVolatileVowel: Boolean = outer.hasVolatileVowel

    override def primarySyntacticCharacteristic: String = outer.primarySyntacticCharacteristic

    override def primaryMorphologicalCharacteristic: String = outer.primaryMorphologicalCharacteristic

    override def initialForm: String = outer.initialForm
  }

  private def newForm(gender: Gender, number: Number.Number, rCase: Case, animacy: Animacy): AdjectiveForm = {
    val form = new AdjectiveForm()
    form._gender = gender
    form.number = number
    form.rCase = rCase
    form._animacy = animacy
    form.isEndingStressed = stressTable.isEndingStressed(form)
    form
  }
}

object Adjective {
  private val primarySyntacticCharacteristic = raw"п"
  private val regex = CommonName.regexPattern.format(primarySyntacticCharacteristic).r

  def apply(record: String) = {
    record match {
      case regex(
      initialForm,
      stressedVowelsPositions,
      primarySyntacticCharacteristic,
      primaryMorphologicCharacteristic,
      declensionSubtype,
      volatileVowelIndicator,
      primaryStressType,
      secondaryStressType
      ) => {
        val primMorphChar = Utils.firstNotNull(primaryMorphologicCharacteristic, primarySyntacticCharacteristic)
        val declensionType = DeclensionType(primMorphChar)
        val volatileVowel = volatileVowelIndicator == "*"
        val adjective = new Adjective()
        adjective._stem = CommonName.stemmer.getStem(declensionType, initialForm)
        adjective._hasVolatileVowel = volatileVowel
        adjective._declensionType = declensionType
        adjective._declensionSubtype = declensionSubtype.toInt
        adjective._primaryStressType = PrimaryStressType(primaryStressType)
        adjective._secondaryStressType = SecondaryStressType(secondaryStressType)
        adjective._primarySyntacticCharacteristic = primarySyntacticCharacteristic
        adjective._primaryMorphologicalCharacteristic = primMorphChar
        adjective._initialForm = initialForm

        adjective._inflectedForms = for (
          gender <- Gender.commonValues.toSeq;
          number <- Number.values.toSeq;
          rCase <- Case.values.toSeq;
          animacy <- Animacy.values.toSeq
        )
          yield {
            val form = adjective.newForm(gender, number, rCase, animacy)
            form -> CommonName.inflectedForm.produce(form)
          }

        adjective
      }
      case _ => ???
    }
  }
}