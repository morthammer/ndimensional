package com.ndim

import java.net.URL
import scala.collection.immutable.Set
import scala.io.Source

trait Value
trait NumberValue extends Value
case object IntegerValue extends NumberValue
case object RealValue extends NumberValue
case object DateTimeValue extends Value
case object StringValue extends Value
case object BooleanValue extends Value

trait Category
case object States extends Category
case object Colors extends Category
//The column meets the criteria for a Category, but the specific Category can't be determined
case object UnknownCategory extends Category

case class DatumAndColIndex(datum: String, columnIndex: Int)
case class ColumnMeta(value: Value, category: Option[Category])
case class Column(allValues: Seq[String], distinctValues: Set[String])

/**
  * Class to parse csv file.
  *
  * @param url the location of the csv file to be parsed
  */

class CSVParser(url: URL) {

  /**
    * Generates a Map of column index to ColumnMeta of a csv file.
    * ColumnMeta contains a value and an optional category.
    * Value is type of data in the column, and category is a further description of the type.
    *
    * @return Map[Int, ColumnMeta]
    */
  def csvColumnMetaData = {
    val dataColIndex =
    for {
      line <- handleHeader(Source.fromURL(url).getLines().toSeq)
      valWithColIndex <- line.split(",").toSeq.map(_.trim).zipWithIndex
    } yield DatumAndColIndex.tupled(valWithColIndex)

    dataColIndex.groupBy(_.columnIndex).map {
      case(k, v) =>
        val allData = v.map(_.datum).filter(_.nonEmpty)
        val uniqueData = v.map(_.datum).filter(_.nonEmpty).toSet
        k -> ColumnMeta(ColumnType.determineColTypeValue(uniqueData), ColumnCategory.determineCategory(Column(allData, uniqueData)))
    }
  }

  /**
    * Formats the meta data generated from the csv with lines of data from csv and outputs them to stdout
    *
    * @param linesOfData lines of data to be outputted to stdout
    */
  def printLinesWithMeta(linesOfData: Int): Unit = {
    val meta = csvColumnMetaData
    val lines = Source.fromURL(url).getLines().take(linesOfData).toSeq

    val headerRow =
      lines.headOption.map{
      s =>
        (for{
          i <- s.split(",").indices
        } yield meta.get(i).fold("NO META")(m => s"VALUE [${m.value}] CATEGORY[${m.category}]")).mkString("", ",", "\n")
      }
    headerRow.foreach(hr => println(hr + lines.mkString(sep = "\n")))
  }

  /**
    * Handles the case of a header present on the csv file.
    * This method assumes a first column of all StringValue is a header.
    * This is a naive assumption.
    *
    * @param rows the raw data
    * @return rows without a heade
    */
  def handleHeader(rows: Seq[String]) = {
    if(ColumnType.rawValuesToTypes(rows.head.split(",").map(_.trim).toSet) == Set(StringValue))
      rows.tail
    else
      rows
  }
}

/**
  * Object to determine the type of a column from the column's data
  *
  */
object ColumnType{
  /**
    * Method to determine the TypeValue from the rawValues of a column
    *
    * @param rawValues a set of the value of the column
    * @return a TypeValue
    */
  def determineColTypeValue(rawValues: Set[String]) =
    rawValuesToTypes andThen reduceTypes apply(rawValues)

  /**
    * Function that takes the Set[String] rawValues and generates a Set[RawValue]
    *
    */
  val rawValuesToTypes: Set[String] => Set[Value] = (rawValues: Set[String]) => {
    rawValues.collect{
      case s if isReal(s) => RealValue
      case s if isInteger(s) => IntegerValue
      case s if isBoolean(s) => BooleanValue
      case s if isDateTime(s) => DateTimeValue
      case _ => StringValue
    }
  }

  /**
    * Function that reduces a Set[Value] to a single value.  If the Set[Value] was reduced to 2 NumberValues we revert to RealValue
    *
    */
  val reduceTypes: Set[Value] => Value = (types: Set[Value]) => {
    types.toList match {
      case h::Nil  => h
      case List(_:NumberValue, _:NumberValue) => RealValue
      case _ => StringValue
    }
  }

  def isReal(str: String) = str matches "[-+]?\\d*\\.{1}\\d+"

  def isInteger(str: String) = str matches "[-+]?\\d+.?"

  def isDateTime(str: String) = str matches "\\d{1,2}[\\/]{1}\\d{1,2}[\\/]{1}\\d{4}"

  def isBoolean(str: String) = {
    str toUpperCase() match {
      case "TRUE" | "FALSE" => true
      case _ => false
    }
  }
}

/**
  * Object to generate the category of a column
  *
  */
object ColumnCategory{
  val stateAbbreviations = "AK,AL,AZ,AR,CA,CO,CT,DE,FL,GA,HI,ID,IL,IN,IA,KS,KY,LA,ME,MD,MA,MI,MN,MS,MO,MT,NE,NV,NH,NJ,NM,NY,NC,ND,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VT,VA,WA,WV,WI,WY"
  val colors = "RED,PINK,ORANGE,YELLOW,PURPLE,GREEN,BLUE,INDIGO,BROWN"

  /**
    * Method to test if the column values are a predefined category
    *
    * @param vals Set of values from a column of the csv file
    * @param knownCategory a known category to test the values against
    * @return Boolean value representing whether the vals are a member of the category
    */
  def isKnownCategory(vals: Set[String], knownCategory: String) = {
    if(vals.isEmpty || knownCategory.isEmpty)
      false
    else
      vals.forall(v => knownCategory.split(",").toSeq.contains(v.trim.toUpperCase()))
  }

  /**
    * Method to determine if the column can be considered a category.  A column is a category if
    * it has less than 1000 distinct values and the percentage of unique values is less than 10%
    *
    * @param col the data of a column from the csv file
    * @return Boolean representing whether the column can be represented as a category
    */
  def isCategory(col: Column) = {
    val allSize = col.allValues.size
    val distinctSize = col.distinctValues.size
    (distinctSize < 1000) && (distinctSize.toFloat / allSize < .10)
  }

  /**
    *Method to determine a column's category
    *
    * @param col Column represents the column's data
    * @return Option[Category] that represents the category of the column, None if the column is not considered a category
    */
  def determineCategory(col: Column): Option[Category] = {
    if(isCategory(col)) {
      val category =
      col.distinctValues match {
        case c if (isKnownCategory(c, colors)) => Colors
        case c if (isKnownCategory(c, stateAbbreviations)) => States
        case _ => UnknownCategory
      }
      Some(category)
    }
    else
      None
  }
}

object CSVParser extends App{

  def apply(url: URL) = new CSVParser(url)

  val p = new CSVParser(new URL(args(0)))
  p.printLinesWithMeta(10)

}
