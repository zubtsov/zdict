package nlp.dictionary.zaliznyak.feature.enums.conjugation
//aka Наклонение
object Mood extends Enumeration {
  type Mood = Value
  //aka Изъявительное, Условное, Повелительное
  val Indicative, Subjunctive, Imperative = Value
}
