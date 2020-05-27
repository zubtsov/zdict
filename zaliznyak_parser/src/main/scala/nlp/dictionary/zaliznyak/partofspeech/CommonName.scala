package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.feature.common.{HasGender, HasNumber, HasStem, IsPartOfSpeech}
import nlp.dictionary.zaliznyak.feature.declension.{HasAnimacy, HasCase, HasDeclensionTypeAndSubtype, HasInitialForm, HasStressType, HasSyntacticAndMorphologicalCharacteristics}
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType.DeclensionType
import nlp.dictionary.zaliznyak.feature.enums.declension.PrimaryStressType.PrimaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension.SecondaryStressType.SecondaryStressType

//common trait for any Name (aka Имя [существительное/прилагательное/местоимение])
trait CommonName extends HasDeclensionTypeAndSubtype with HasStressType with HasStem with IsPartOfSpeech
  with HasSyntacticAndMorphologicalCharacteristics with HasInitialForm {//todo: remove this trait
  protected var _declensionType: DeclensionType = _
  protected var _declensionSubtype: Int = _
  protected var _primaryStressType: PrimaryStressType = _
  protected var _secondaryStressType: Option[SecondaryStressType] = None
  protected var _stem: String = _
  protected var _hasVolatileVowel: Boolean = _
  protected var _inflectedForms: Seq[(HasGender with HasNumber with HasCase with HasAnimacy, String)] = _

  protected var _primarySyntacticCharacteristic: String = _
  protected var _primaryMorphologicalCharacteristic: String = _
  protected var _initialForm: String = _

  def inflectedForms = _inflectedForms

  override def declensionType = _declensionType

  override def declensionSubtype = _declensionSubtype

  override def declensionSubtype_=(declensionSubtype: Int) = {
    _declensionSubtype = declensionSubtype
  }

  override def primaryStressType: PrimaryStressType = _primaryStressType

  override def secondaryStressType: Option[SecondaryStressType] = _secondaryStressType

  override def stem: String = _stem

  override def hasVolatileVowel: Boolean = _hasVolatileVowel

  override def toString: String = {
    partOfSpeech + ": " + _inflectedForms.map(_._2).mkString(",")
  }

  override def primarySyntacticCharacteristic: String = _primarySyntacticCharacteristic

  override def primaryMorphologicalCharacteristic: String = _primaryMorphologicalCharacteristic

  override def initialForm: String = _initialForm
}
