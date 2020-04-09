package org.zubtsov.dictionary.zaliznyak.attributes

import org.zubtsov.dictionary.zaliznyak.attributes.enums.DeclensionType.DeclensionType

trait HasDeclensionTypeAndSubtype {
  def declensionType: DeclensionType

  def declensionSubtype: Int

  def declensionSubtype_=(declensionSubtype: Int): Unit //todo: think about returning something more useful?
}
