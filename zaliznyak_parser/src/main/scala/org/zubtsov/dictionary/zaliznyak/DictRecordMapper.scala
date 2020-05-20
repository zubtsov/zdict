package org.zubtsov.dictionary.zaliznyak

import org.zubtsov.dictionary.zaliznyak.entities.{NameDictionaryRecord, VerbDictionaryRecord}
import org.zubtsov.dictionary.zaliznyak.partsofspeech.{Adjective, Noun, Verb}

object DictRecordMapper {
  def map(dictrecord: NameDictionaryRecord) = {
    import dictrecord.primarySyntacticCharacteristic

    primarySyntacticCharacteristic match {
      case "м" | "ж" | "с" | "мо" | "жо" | "со" | "мн. одуш." | "мн. неод." => {
        Noun(dictrecord)
      }
      case "п" => {
        Adjective(dictrecord)
      }
      case _ => ???
    }
  }

  def map(dictrecord: VerbDictionaryRecord) = {
    Verb(dictrecord)
  }
}
