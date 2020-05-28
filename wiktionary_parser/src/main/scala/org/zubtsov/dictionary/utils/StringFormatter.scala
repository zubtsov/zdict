package org.zubtsov.dictionary.utils

import java.text.Normalizer

object StringFormatter {

 def normalizeString(str: String): String = {
    Normalizer.normalize(str, Normalizer.Form.NFD).replaceAll("\\p{M}", "")
  }

}
