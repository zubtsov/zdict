package org.zubtsov.dictionary.zaliznyak.partsofspeech

import org.zubtsov.dictionary.zaliznyak.Stem
import org.zubtsov.dictionary.zaliznyak.attributes.{enums, _}
import org.zubtsov.dictionary.zaliznyak.attributes.common.HasStem
import org.zubtsov.dictionary.zaliznyak.attributes.enums.common.Gender
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.Animacy.Animacy
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.Case.Case
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.DeclensionType.DeclensionType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.common.Gender.Gender
import org.zubtsov.dictionary.zaliznyak.attributes.enums.common.Number.Number
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.{Animacy, Case, DeclensionType}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.PrimaryStressType.PrimaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.SecondaryStressType.SecondaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.{Animacy, DeclensionType, Number}
import org.zubtsov.dictionary.zaliznyak.declension.Declension
import org.zubtsov.dictionary.zaliznyak.entities.NameDictionaryRecord
import org.zubtsov.dictionary.zaliznyak.partsofspeech.PartOfSpeech.PartOfSpeech
import org.zubtsov.dictionary.zaliznyak.stress.Stress

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
    form.isEndingStressed = Stress.isEndingStressed(form)
    form
  }
}

object Noun {
  def apply(dictrecord: NameDictionaryRecord) = {
    import dictrecord.{declensionSubtype, primaryMorphologicalCharacteristic, primarySyntacticCharacteristic}

    primarySyntacticCharacteristic match {
      //Существительное
      case "м" | "ж" | "с" | "мо" | "жо" | "со" | "мн. одуш." | "мн. неод." => {
        val declensionType = DeclensionType(primaryMorphologicalCharacteristic)
        val gender = Gender(primarySyntacticCharacteristic)
        val noun = new Noun()
        noun._stem = Stem.getStem(declensionType, dictrecord.initialForm)
        noun._hasVolatileVowel = dictrecord.volatileVowel
        noun._gender = gender
        noun._animacy = Animacy(primarySyntacticCharacteristic).orNull
        noun._declensionType = declensionType
        noun._declensionSubtype = declensionSubtype
        noun._primaryStressType = dictrecord.primaryStressType
        noun._secondaryStressType = dictrecord.secondaryStressType
        noun._primarySyntacticCharacteristic = dictrecord.primarySyntacticCharacteristic
        noun._primaryMorphologicalCharacteristic = dictrecord.primaryMorphologicalCharacteristic
        noun._initialForm = dictrecord.initialForm

        noun._inflectedForms = for (number <- enums.common.Number.values.toSeq;
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