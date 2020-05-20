package org.zubtsov.dictionary.zaliznyak.entities

import org.zubtsov.dictionary.zaliznyak.Utils
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.DeclensionType.DeclensionType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.PrimaryStressType.PrimaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.SecondaryStressType.SecondaryStressType
import org.zubtsov.dictionary.zaliznyak.attributes.enums._
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.{DeclensionType, PrimaryStressType, SecondaryStressType}
//aka Запись словаря для имен
case class NameDictionaryRecord(
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

object NameDictionaryRecord {
  val regex = (
    //начальная форма слова
    raw"^([а-яА-Я\-]+)\s" +
      //позиция ударной гласной
      raw"([0-9]+\.?[0-9]?)\s" +
      //основная синтаксическая характеристика
      raw"(" +
      //имена
      raw"м|ж|с|мо|жо|со|мо\-жо" +
      raw"|п" +
      raw"|мс|мс-п" +
      raw"|мн\. одуш\.|мн\. неод\." +
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
    ).r

  def apply(record: String): NameDictionaryRecord = {
    record match {
      case regex(
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

        NameDictionaryRecord(initialForm,
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
