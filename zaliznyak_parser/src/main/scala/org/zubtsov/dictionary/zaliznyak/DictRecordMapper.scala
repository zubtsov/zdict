package org.zubtsov.dictionary.zaliznyak

import org.zubtsov.dictionary.zaliznyak.entities.DictionaryRecord
import org.zubtsov.dictionary.zaliznyak.partsofspeech.{Adjective, Noun}

object DictRecordMapper {
  def map(dictrecord: DictionaryRecord) = {
    import dictrecord.primarySyntacticCharacteristic

    primarySyntacticCharacteristic match {
      //Существительное
      case "м" | "ж" | "с" | "мо" | "жо" | "со" | "мн. одуш." | "мн. неод." => {
        Noun(dictrecord)
      }
      case "п" => {
        Adjective(dictrecord)
      }
      case _ => ???
    }
  }
}
