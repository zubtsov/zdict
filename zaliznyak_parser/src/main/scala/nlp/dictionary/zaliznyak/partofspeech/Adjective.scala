package nlp.dictionary.zaliznyak.partofspeech

import nlp.dictionary.zaliznyak.declension.InflectedFormOfName
import nlp.dictionary.zaliznyak.feature.common.IsPartOfSpeech
import nlp.dictionary.zaliznyak.feature.enums.common.Gender.Gender
import nlp.dictionary.zaliznyak.feature.enums.common.Number.Number
import nlp.dictionary.zaliznyak.feature.enums.common.{Gender, Number}
import nlp.dictionary.zaliznyak.feature.enums.declension.Animacy.Animacy
import nlp.dictionary.zaliznyak.feature.enums.declension.Case.Case
import nlp.dictionary.zaliznyak.feature.enums.declension.DeclensionType.DeclensionType
import nlp.dictionary.zaliznyak.feature.enums.declension.PrimaryStressType.PrimaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension.SecondaryStressType.SecondaryStressType
import nlp.dictionary.zaliznyak.feature.enums.declension._
import nlp.dictionary.zaliznyak.helper.Utils
import nlp.dictionary.zaliznyak.partofspeech.PartOfSpeech.PartOfSpeech
import nlp.dictionary.zaliznyak.stress.WordWithStress

class Adjective extends CommonName {
  outer =>

  override def partOfSpeech: PartOfSpeech = PartOfSpeech.Adjective

  // todo: why can't we define type as mixin of multiple traits and inherit it?
  class AdjectiveForm private[Adjective](n: Number, c: Case, g: Gender, a: Animacy) extends InflectedFormOfName
    with WordWithStress with IsPartOfSpeech {
    override def rCase: Case = c

    override def number: Number = n

    override def gender: Gender = g

    override def animacy: Animacy = a

    override def stem: String = outer.stem

    override def partOfSpeech: PartOfSpeech = outer.partOfSpeech

    override def declensionType: DeclensionType = outer.declensionType

    override def declensionSubtype: Int = outer.declensionSubtype

    override def declensionSubtype_=(declensionSubtype: Int): Unit = outer.declensionSubtype_=(declensionSubtype)

    override def primaryStressType: PrimaryStressType = outer.primaryStressType

    override def secondaryStressType: Option[SecondaryStressType] = outer.secondaryStressType

    override def hasVolatileVowel: Boolean = outer.hasVolatileVowel

    override def primarySyntacticCharacteristic: String = outer.primarySyntacticCharacteristic

    override def primaryMorphologicalCharacteristic: String = outer.primaryMorphologicalCharacteristic

    override def initialForm: String = outer.initialForm

    val form: String = inflectedForm()

    override def toString: String = form
  }

}

object Adjective {
  private val primarySyntacticCharacteristic = raw"п"
  private val regex = CommonName.regexPattern.format(primarySyntacticCharacteristic).r

  def apply(record: String) = {
    record match {
      case regex(
      initialForm,
      stressedVowelsPositions,
      primarySyntacticCharacteristic,
      primaryMorphologicCharacteristic,
      declensionSubtype,
      volatileVowelIndicator,
      primaryStressType,
      secondaryStressType
      ) => {
        val primMorphChar = Utils.firstNotNull(primaryMorphologicCharacteristic, primarySyntacticCharacteristic)
        val declensionType = DeclensionType(primMorphChar)
        val volatileVowel = volatileVowelIndicator == "*"
        val adjective = new Adjective()
        adjective._stem = CommonName.stemmer.getStem(declensionType, initialForm)
        adjective._hasVolatileVowel = volatileVowel
        adjective._declensionType = declensionType
        adjective._declensionSubtype = declensionSubtype.toInt
        adjective._primaryStressType = PrimaryStressType(primaryStressType)
        adjective._secondaryStressType = SecondaryStressType(secondaryStressType)
        adjective._primarySyntacticCharacteristic = primarySyntacticCharacteristic
        adjective._primaryMorphologicalCharacteristic = primMorphChar
        adjective._initialForm = initialForm

        adjective._inflectedForms = for (
          gender <- Gender.commonValues.toSeq;
          number <- Number.values.toSeq;
          rCase <- Case.values.toSeq;
          animacy <- Animacy.values.toSeq
        )
          yield {
            new adjective.AdjectiveForm(number, rCase, gender, animacy)
          }

        adjective
      }
      case _ => ???
    }
  }
}