package nlp.dictionary.zaliznyak

import nlp.dictionary.zaliznyak.entity.{NameDictionaryRecord, VerbDictionaryRecord}
import nlp.dictionary.zaliznyak.partofspeech.{Adjective, Noun, Verb}

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
