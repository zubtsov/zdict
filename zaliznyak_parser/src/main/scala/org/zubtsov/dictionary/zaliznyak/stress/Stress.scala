package org.zubtsov.dictionary.zaliznyak.stress

import org.zubtsov.dictionary.zaliznyak.attributes.common.HasNumber
import org.zubtsov.dictionary.zaliznyak.attributes.enums.declension.{Case, PrimaryStressType, SecondaryStressType}
import org.zubtsov.dictionary.zaliznyak.attributes.enums.common
import org.zubtsov.dictionary.zaliznyak.attributes.declension.{HasCase, HasStressType}

//aka Ударение
object Stress {
  def isStemStressed(parameters: HasStressType with HasNumber with HasCase): Boolean = {
    import parameters._

    val initialStress = primaryStressType match {
      case PrimaryStressType.a => true //a
      case PrimaryStressType.b => false //b todo: кроме случаев когда stem не содержит гласную
      case PrimaryStressType.c => number match {
        case common.Number.Singular => true
        case common.Number.Plural => false
        case _ => ???
      } //c
      case PrimaryStressType.d => number match {
        case common.Number.Singular => false
        case common.Number.Plural => true
        case _ => ???
      }
      case PrimaryStressType.e => number match {
        case common.Number.Singular => true
        case common.Number.Plural => if (rCase == Case.Nominative) true else false
        case _ => ???
      } //e
      case PrimaryStressType.f => if (number == common.Number.Plural && rCase == Case.Nominative) true else false //f
      case _ => ???
    }
    if (secondaryStressType.isDefined) {
      secondaryStressType.get match {
        case SecondaryStressType.Apostrophe => primaryStressType match {
          case PrimaryStressType.b => if (rCase == Case.Instrumental && number == common.Number.Singular) true else initialStress //b todo: кроме случаев когда stem не содержит гласную
          case PrimaryStressType.d | PrimaryStressType.f => if (rCase == Case.Accusative && number == common.Number.Singular) true else initialStress
          case _ => ???
        }
        case SecondaryStressType.DoubleApostrophe => primaryStressType match {
          case PrimaryStressType.f => if (rCase == Case.Instrumental && number == common.Number.Singular) true else initialStress //todo: параллельно со схемой е
          case _ => ???
        }
        case _ => ???
      }
    }
    else
      initialStress
  }

  def isEndingStressed(parameters: HasStressType with HasNumber with HasCase): Boolean = {
    !isStemStressed(parameters)
  }
}
