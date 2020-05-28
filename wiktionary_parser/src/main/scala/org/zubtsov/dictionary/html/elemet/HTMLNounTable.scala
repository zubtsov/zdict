package org.zubtsov.dictionary.html.elemet

class HTMLNounTable extends HTMLTable {

  override def headerSize: Int = 3

  override def header: Array[String] = {
    Array(".*падеж.*", ".*ед\\..*ч\\..*", ".*мн\\..*ч\\..*")
  }
}
