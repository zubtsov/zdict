package nlp.dictionary.zaliznyak.feature.declension

import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType.DeclensionType

trait HasDeclensionTypeAndSubtype {
  def declensionType: DeclensionType

  def declensionSubtype: Int

  def declensionSubtype_=(declensionSubtype: Int): Unit //todo: think about returning something more useful?
}
