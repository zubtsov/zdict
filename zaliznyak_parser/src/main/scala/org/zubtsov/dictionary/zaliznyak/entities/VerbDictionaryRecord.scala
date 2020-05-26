package org.zubtsov.dictionary.zaliznyak.entities

import org.zubtsov.dictionary.zaliznyak.attributes.enums.conjugation.{Aspect, Transitivity}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.conjugation.Aspect.Aspect
import org.zubtsov.dictionary.zaliznyak.attributes.enums.conjugation.Transitivity.Transitivity

//aka Запись словаря для глаголов
case class VerbDictionaryRecord(
                                 initialForm: String,
                                 stressedVowelsPositions: String,
                                 aspect: Aspect,
                                 transitive: Transitivity,
                                 conjugationType: Int,
                                 volatileVowelIndicator: String,
                                 stressType: String
                               )

object VerbDictionaryRecord {
  val regex = (
    //начальная форма глагола (инфинитив)
    raw"^([а-яА-Я\-]+)\s" +
      //позиция ударной гласной
      raw"([0-9]+\.?[0-9]?)\s" +
      //вид
      raw"(св|нсв|св-нсв)\s" +
      //переходность
      raw"(нп)?\s?" +
      //тип спряжения
      raw"([0-9])+" +
      //беглая гласная
      raw"(\*{1,2})?" +
      //схема ударения
      raw"([а|в|с]/?[а|в|с]?)" +
      //      raw"[\w\W]*" +
      "$"
    ).r

  def apply(record: String): VerbDictionaryRecord = {
    import org.zubtsov.dictionary.zaliznyak.helpers.Utils.RussianWord
    record match {
      case regex(
      infinitive,
      stressedVowelsPositions,
      aspect,
      transitive,
      conjugationIndex,
      volatileVowelIndicator,
      stressType
      ) => {
        val reflexive = infinitive.endsWithAnyOf("ся", "сь")

        VerbDictionaryRecord(
          infinitive,
          stressedVowelsPositions,
          Aspect(aspect),
          Transitivity(reflexive, Option(transitive)),
          conjugationIndex.toInt,
          volatileVowelIndicator,
          stressType
        )
      }
      case _ => ???
    }
  }
}