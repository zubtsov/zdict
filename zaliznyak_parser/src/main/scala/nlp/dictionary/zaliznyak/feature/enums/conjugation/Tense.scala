package nlp.dictionary.zaliznyak.feature.enums.conjugation
//aka Время
object Tense extends Enumeration {
  type Tense = Value
  //aka Прошедшее, Настоящее, Будущее
  val Past, Present, Future = Value
}
