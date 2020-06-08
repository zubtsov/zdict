package org.zubtsov.dictionary.html.elemet

class HTMLVerbTable extends HTMLTable {

  override def headerSize: Int = 3

  override def header: Array[String] = {
    Array(".*наст\\.|будущ\\..*", ".*прош\\..*", ".*повелит\\..*")
  }
}