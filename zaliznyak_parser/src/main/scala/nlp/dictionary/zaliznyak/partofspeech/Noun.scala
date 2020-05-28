package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.Stem
import nlp.dictionary.zaliznyak.declension.Declension
import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber, HasStem, IsPartOfSpeech}
import nlp.dictionary.zaliznyak.feature.declension._
import nlp.dictionary.zaliznyak.feature.enums.common
import nlp.dictionary.zaliznyak.feature.enums.common.Gender
import nlp.dictionary.zaliznyak.feature.enums.common.Gender.Gender
import nlp.dictionary.zaliznyak.feature.enums.common.Number.Number
import nlp.dictionary.zaliznyak.feature.enums.declension.Animacy.Animacy
import nlp.dictionary.zaliznyak.feature.enums.declension.Case.Case
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType.DeclensionType
import nlp.dictionary.zaliznyak.feature.enums.declension.PrimaryStressType.PrimaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension.SecondaryStressType.SecondaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension._
import nlp.dictionary.zaliznyak.helper.Utils
import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech

//aka Существительное
class Noun private() extends CommonName with HasGender with HasAnimacy {
  outer =>

  override def partOfSpeech: PartOfSpeech = PartOfSpeech.Noun

  private var _gender: Gender = _
  private var _animacy: Animacy = _

  override def gender: Gender = _gender

  override def animacy: Animacy = _animacy

  //todo: perhaps, create one more interface for CommonName and move it to package object
  private[Noun] trait NounSpecificAttributes extends HasDeclensionTypeAndSubtype with HasStressType with HasStem with IsPartOfSpeech
    with HasSyntacticAndMorphologicalCharacteristics with HasInitialForm with HasGender with HasAnimacy

  private[Noun] trait NounFormSpecificAttributes extends HasNumber with HasCase with HasStress

  //todo: remove fields from some (all?) traits
  // todo: why can't we define type as mixin of multiple traits and inherit it?
  class NounForm private[Noun]() extends NounSpecificAttributes
    with NounFormSpecificAttributes {
    override def gender: Gender = outer.gender

    override def animacy: Animacy = outer.animacy

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

  private def newForm(number: Number, rCase: Case): NounForm = {
    val form = new NounForm()
    form.number = number
    form.rCase = rCase
    form.isEndingStressed = stressTable.isEndingStressed(form)
    form
  }
}

object Noun {
  private val primarySyntacticCharacteristic = raw"м|ж|с|мо|жо|со|мо\-жо"
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
        val noun = new Noun()
        val primMorphChar = Utils.firstNotNull(primaryMorphologicCharacteristic, primarySyntacticCharacteristic)
        val declensionType = DeclensionType(primMorphChar)
        val gender = Gender(primarySyntacticCharacteristic)
        val volatileVowel = volatileVowelIndicator == "*"
        noun._stem = Stem.getStem(declensionType, initialForm)
        noun._hasVolatileVowel = volatileVowel
        noun._gender = gender
        noun._animacy = Animacy(primarySyntacticCharacteristic).orNull
        noun._declensionType = declensionType
        noun._declensionSubtype = declensionSubtype.toInt
        noun._primaryStressType = PrimaryStressType(primaryStressType)
        noun._secondaryStressType = SecondaryStressType(secondaryStressType)
        noun._primarySyntacticCharacteristic = primarySyntacticCharacteristic
        noun._primaryMorphologicalCharacteristic = primMorphChar
        noun._initialForm = initialForm

        noun._inflectedForms = for (number <- common.Number.values.toSeq;
                                    rCase <- Case.values.toSeq)
          yield {
            val form = noun.newForm(number, rCase)
            form -> Declension.inflectedForm(form)
          }

        noun
      }
      case _ => ???
    }
  }
}