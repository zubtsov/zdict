package org.zubtsov.dictionary.zaliznyak.attributes

import org.zubtsov.dictionary.zaliznyak.partsofspeech.PartOfSpeech.PartOfSpeech

trait IsPartOfSpeech {
  def partOfSpeech: PartOfSpeech
}
