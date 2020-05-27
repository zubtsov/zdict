package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.Stem
import nlp.dictionary.zaliznyak.declension.Declension
import nlp.dictionary.zaliznyak.entity.NameDictionaryRecord
import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber, HasStem, IsPartOfSpeech}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasInitialForm, HasStress, HasStressType, HasSyntacticAndMorphologicalCharacteristics}
import nlp.dictionary.zaliznyak.feature.enums.common.Gender.Gender
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.Animacy.Animacy
import nlp.dictionary.zaliznyak.feature.enums.declension.Case.Case
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType.DeclensionType
import nlp.dictionary.zaliznyak.feature.enums.declension.PrimaryStressType.PrimaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension.SecondaryStressType.SecondaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension.{Animacy, Case, DeclensionType}
import nlp.dictionary.zaliznyak.helper.Utils
import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech
import nlp.dictionary.zaliznyak.stress.Stress

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
    form.isEndingStressed = Stress.isEndingStressed(form)
    form
  }
}

object Adjective {
  def apply(dictrecord: NameDictionaryRecord) = {
    import dictrecord.{declensionSubtype, primaryMorphologicalCharacteristic, primarySyntacticCharacteristic}

    primarySyntacticCharacteristic match {
      //Прилагательное
      case "п" => {
        val declensionType = DeclensionType(Utils.firstNotNull(primaryMorphologicalCharacteristic, primarySyntacticCharacteristic))
        val adjective = new Adjective()
        adjective._stem = Stem.getStem(declensionType, dictrecord.initialForm)
        adjective._hasVolatileVowel = dictrecord.volatileVowel
        adjective._declensionType = declensionType
        adjective._declensionSubtype = declensionSubtype
        adjective._primaryStressType = dictrecord.primaryStressType
        adjective._secondaryStressType = dictrecord.secondaryStressType
        adjective._primarySyntacticCharacteristic = dictrecord.primarySyntacticCharacteristic
        adjective._primaryMorphologicalCharacteristic = dictrecord.primaryMorphologicalCharacteristic
        adjective._initialForm = dictrecord.initialForm

        adjective._inflectedForms = for (
          gender <- Gender.commonValues.toSeq;
          number <- Number.values.toSeq;
          rCase <- Case.values.toSeq;
          animacy <- Animacy.values.toSeq
        )
          yield {
            val form = adjective.newForm(gender, number, rCase, animacy)
            form -> Declension.inflectedForm(form)
          }

        adjective
      }
      case _ => ???
    }
  }
}