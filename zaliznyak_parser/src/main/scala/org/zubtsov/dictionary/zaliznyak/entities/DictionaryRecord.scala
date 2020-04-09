package org.zubtsov.dictionary.zaliznyak.entities

import org.zubtsov.dictionary.zaliznyak.Utils
import org.zubtsov.dictionary.zaliznyak.attributes.enums.DeclensionType.DeclensionType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.PrimaryStressType.PrimaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.SecondaryStressType.SecondaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums._

case class DictionaryRecord(
                             initialForm: String,
                             stressedVowelsPositions: String,
                             primarySyntacticCharacteristic: String,
                             primaryMorphologicalCharacteristic: String,
                             declensionSubtype: Int,
                             volatileVowelIndicator: String,
                             primaryStressType: PrimaryStressType,
                             secondaryStressType: Option[SecondaryStressType],
                             declensionType: DeclensionType,
                             volatileVowel: Boolean
                           )

object DictionaryRecord {
  val DictionaryRecordRegex = (
    //начальная форма слова
    raw"^([а-яА-Я\-]+)\s" +
      //позиция ударной гласной
      raw"([0-9]+\.?[0-9]?)\s" +
      //модификатор склонения 1
      raw"(" +
      //имена
      raw"м|ж|с|мо|жо|со|мо\-жо" +
      raw"|п" +
      raw"|мс|мс-п" +
      raw"|мн\. одуш\.|мн\. неод\." +
      //глаголы
      //    raw"|св|нсв|св\-нсв" +
      //проч
      //    raw"|н|числ\.|числ\.-п|част\." +
      raw")\s" +
      //модификатор склонения 2
      raw"(" +
      //имена
      raw"м|ж|с|мо|жо|со|мо\-жо" +
      raw"|п" +
      raw"|мс|мс-п" +
      raw"|мн\. одуш\.|мн\. неод\." +
      //глаголы
      //    raw"|св|нсв|св\-нсв" +
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
    ).r

  def apply(record: String): DictionaryRecord = {
    record match {
      case DictionaryRecordRegex(
      initialForm,
      stressedVowelsPositions,
      primarySyntacticCharacteristic,
      primaryMorphologicCharacteristic,
      declensionSubtype,
      volatileVowelIndicator,
      primaryStressScheme,
      secondaryStressScheme
      ) => {
        val primMorphChar = Utils.firstNotNull(primaryMorphologicCharacteristic, primarySyntacticCharacteristic)
        val declensionType = DeclensionType(primMorphChar)

        DictionaryRecord(initialForm,
          stressedVowelsPositions,
          primarySyntacticCharacteristic,
          primMorphChar,
          declensionSubtype.toInt,
          volatileVowelIndicator,
          PrimaryStressType(primaryStressScheme),
          SecondaryStressType(secondaryStressScheme),
          declensionType,
          volatileVowelIndicator == "*")
      }
      case _ => ???
    }
  }
}
