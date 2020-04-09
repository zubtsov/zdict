package org.zubtsov.dictionary.zaliznyak.stress

import org.zubtsov.dictionary.zaliznyak.attributes.enums.{Case, Number, PrimaryStressType, SecondaryStressType}
import org.zubtsov.dictionary.zaliznyak.attributes.{HasCase, HasNumber, HasStressType}

//aka Ударение
object Stress {
  def isStemStressed(parameters: HasStressType with HasNumber with HasCase): Boolean = {
    import parameters._

    val initialStress = primaryStressType match {
      case PrimaryStressType.a => true //a
      case PrimaryStressType.b => false //b todo: кроме случаев когда stem не содержит гласную
      case PrimaryStressType.c => number match {
        case Number.Singular => true
        case Number.Plural => false
        case _ => ???
      } //c
      case PrimaryStressType.d => number match {
        case Number.Singular => false
        case Number.Plural => true
        case _ => ???
      }
      case PrimaryStressType.e => number match {
        case Number.Singular => true
        case Number.Plural => if (rCase == Case.Nominative) true else false
        case _ => ???
      } //e
      case PrimaryStressType.f => if (number == Number.Plural && rCase == Case.Nominative) true else false //f
      case _ => ???
    }
    if (secondaryStressType.isDefined) {
      secondaryStressType.get match {
        case SecondaryStressType.Apostrophe => primaryStressType match {
          case PrimaryStressType.b => if (rCase == Case.Instrumental && number == Number.Singular) true else initialStress //b todo: кроме случаев когда stem не содержит гласную
          case PrimaryStressType.d | PrimaryStressType.f => if (rCase == Case.Accusative && number == Number.Singular) true else initialStress
          case _ => ???
        }
        case SecondaryStressType.DoubleApostrophe => primaryStressType match {
          case PrimaryStressType.f => if (rCase == Case.Instrumental && number == Number.Singular) true else initialStress //todo: параллельно со схемой е
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
