package org.zubtsov.dictionary.zaliznyak.partsofspeech

import org.zubtsov.dictionary.zaliznyak.attributes.enums.Animacy.Animacy
import org.zubtsov.dictionary.zaliznyak.attributes.enums.Case.Case
import org.zubtsov.dictionary.zaliznyak.attributes.enums.DeclensionType.DeclensionType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.Gender.Gender
import org.zubtsov.dictionary.zaliznyak.attributes.enums.PrimaryStressType.PrimaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.SecondaryStressType.SecondaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.{Animacy, Case, DeclensionType, Gender, Number}
import org.zubtsov.dictionary.zaliznyak.attributes._
import org.zubtsov.dictionary.zaliznyak.attributes.common.HasStem
import org.zubtsov.dictionary.zaliznyak.declension.Declension
import org.zubtsov.dictionary.zaliznyak.entities.DictionaryRecord
import org.zubtsov.dictionary.zaliznyak.partsofspeech.PartOfSpeech.PartOfSpeech
import org.zubtsov.dictionary.zaliznyak.stress.Stress
import org.zubtsov.dictionary.zaliznyak.{Stem, Utils}

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
  def apply(dictrecord: DictionaryRecord) = {
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