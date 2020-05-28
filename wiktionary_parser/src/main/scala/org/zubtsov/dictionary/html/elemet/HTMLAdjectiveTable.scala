package org.zubtsov.dictionary.html.elemet

class HTMLAdjectiveTable extends HTMLTable {

  override def headerSize: Int = 6

  override def header: Array[String] = {
    Array(".*падеж.*", ".*ед\\..*ч\\..*", ".*мн\\..*ч\\..*", ".*муж\\..*р\\..*", ".*ср\\..*р\\..*", ".*жен\\..*р\\..*")
  }
}