package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.feature.common.{HasNumber, IsPartOfSpeech}
import nlp.dictionary.zaliznyak.feature.declension._
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType.DeclensionType
import nlp.dictionary.zaliznyak.feature.enums.declension.PrimaryStressType.PrimaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension.SecondaryStressType.SecondaryStressType

//common trait for any Name (aka Имя [существительное/прилагательное/местоимение])
trait CommonName extends HasDeclensionTypeAndSubtype with HasStressType with IsPartOfSpeech
  with HasSyntacticAndMorphologicalCharacteristics with HasInitialForm { //todo: remove this trait

  protected var _declensionType: DeclensionType = _
  protected var _declensionSubtype: Int = _
  protected var _primaryStressType: PrimaryStressType = _
  protected var _secondaryStressType: Option[SecondaryStressType] = None
  protected var _hasVolatileVowel: Boolean = _
  protected var _inflectedForms: Seq[HasGender with HasNumber with HasCase with HasAnimacy] = _

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

  override def toString: String = {
    partOfSpeech + ": " + _inflectedForms.mkString(",")
  }

  override def primarySyntacticCharacteristic: String = _primarySyntacticCharacteristic

  override def primaryMorphologicalCharacteristic: String = _primaryMorphologicalCharacteristic

  override def initialForm: String = _initialForm
}

object CommonName {
  protected[partofspeech] var regexPattern = raw"^([а-яА-Я\-]+)\s" +
    //позиция ударной гласной
    raw"([0-9]+\.?[0-9]?)\s" +
    //основная синтаксическая характеристика
    raw"(" +
    //имена
    raw"%s" +
    //    raw"|п" +
    //    raw"|мс|мс-п" +
    //    raw"|мн\. одуш\.|мн\. неод\." +
    //проч
    //    raw"|н|числ\.|числ\.-п|част\." +
    raw")\s" +
    //основная морфологическая характеристика
    raw"(" +
    //имена
    raw"м|ж|с|мо|жо|со|мо\-жо" +
    raw"|п" +
    raw"|мс|мс-п" +
    raw"|мн\. одуш\.|мн\. неод\." +
    //проч
    //    raw"|н|числ\.|числ\.-п|част\." +
    raw")?\s?" +
    //индекс типа склонения
    raw"([0-8])?" +
    //беглая гласная
    raw"(\*{1,2})?" +
    //схема ударения
    raw"([а|в|с|е|D|F])?" +
    //второстепенная схема ударения (штрих)
    raw"('{1,2})?" +
    //      raw"[\w\W]*" +
    "$"
}