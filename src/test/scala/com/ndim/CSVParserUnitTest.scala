package com.ndim
import org.scalatest.{Matchers, WordSpec}


class CSVParserUnitTest extends WordSpec with Matchers {

  "CSVParser" should {
    val csvParser = new CSVParser(getClass.getResource("/test.csv"))
    "Handle the header" in {
      csvParser.handleHeader(Seq("Header1, Header2, Header3", "data1, .54463, 123")) shouldBe Seq("data1, .54463, 123")
      csvParser.handleHeader(Seq("data1, .54463, 123", "data2, .2, 122")) shouldBe Seq("data1, .54463, 123", "data2, .2, 122")
    }
    "Construct csv meta data for test csv" in {
      csvParser.csvColumnMetaData shouldBe Map(0 -> ColumnMeta(RealValue,Some(UnknownCategory)), 5 -> ColumnMeta(StringValue,Some(Colors)), 1 -> ColumnMeta(RealValue,Some(UnknownCategory)), 2 -> ColumnMeta(RealValue,Some(UnknownCategory)), 3 -> ColumnMeta(RealValue,Some(UnknownCategory)), 4 -> ColumnMeta(StringValue,Some(UnknownCategory)))
    }
  }
  "ColumnType" should{
    "Identify datum when integer" in {
      //positive test cases
      ColumnType.isInteger("12345") shouldBe true
      ColumnType.isInteger("+12345") shouldBe true
      ColumnType.isInteger("-12345") shouldBe true
      ColumnType.isInteger("0") shouldBe true
      ColumnType.isInteger("1.") shouldBe true

      //negative test cases
      ColumnType.isInteger("") shouldBe false
      ColumnType.isInteger("1234.5") shouldBe false
      ColumnType.isInteger("1234.00000") shouldBe false
      ColumnType.isInteger("hello") shouldBe false
      ColumnType.isInteger("^&*(^") shouldBe false
      ColumnType.isInteger("12,345") shouldBe false
    }
    "Identify datum when float" in {
      //positive test cases
      ColumnType.isReal(".533") shouldBe true
      ColumnType.isReal("1234.5") shouldBe true
      ColumnType.isReal("+1234.5") shouldBe true
      ColumnType.isReal("-1234.5") shouldBe true
      ColumnType.isReal("1234.00000") shouldBe true

      //negative test cases
      ColumnType.isReal("") shouldBe false
      ColumnType.isReal("1.") shouldBe false
      ColumnType.isReal("12345") shouldBe false
      ColumnType.isReal("hello") shouldBe false
      ColumnType.isReal("^&*(^") shouldBe false
      ColumnType.isReal("0") shouldBe false
      ColumnType.isReal("1,234.5") shouldBe false
    }
    "Identify datum when date time" in {
      //positive test cases
      ColumnType.isDateTime("12/10/2014") shouldBe true
      ColumnType.isDateTime("01/01/2014") shouldBe true
      ColumnType.isDateTime("1/1/2014") shouldBe true

      //not supported formats
      ColumnType.isDateTime("") shouldBe false
      ColumnType.isDateTime("12-10-2014") shouldBe false
      ColumnType.isDateTime("1-1-2014") shouldBe false
      ColumnType.isDateTime("01-01-2014") shouldBe false
      ColumnType.isDateTime("01/01/14") shouldBe false
      ColumnType.isDateTime("001/001/14") shouldBe false
      ColumnType.isDateTime("01/01/014") shouldBe false
      ColumnType.isDateTime("1-1-14") shouldBe false
      ColumnType.isDateTime("1/1/2014 11:01:01.000") shouldBe false
      ColumnType.isDateTime("22.12.1978") shouldBe false
      ColumnType.isDateTime("30-June 2008") shouldBe false
      ColumnType.isDateTime("July 1st, 2008") shouldBe false
    }
    "Identify datum when booleam" in {
      //positive test cases
      ColumnType.isBoolean("true") shouldBe true
      ColumnType.isBoolean("false") shouldBe true
      ColumnType.isBoolean("TRUE") shouldBe true
      ColumnType.isBoolean("FALSE") shouldBe true

      //negative test cases
      ColumnType.isBoolean("") shouldBe false
      ColumnType.isBoolean("1") shouldBe false
      ColumnType.isBoolean("0") shouldBe false
      ColumnType.isBoolean("") shouldBe false
      ColumnType.isBoolean("content") shouldBe false
    }
    "Determine the correct type for a Set" in {
      ColumnType.determineColTypeValue(Set("12341", "+12341.333", "0")) shouldBe RealValue
      ColumnType.determineColTypeValue(Set("-12341", "12341.333", "0")) shouldBe RealValue
      ColumnType.determineColTypeValue(Set("12341", "12341.333", "0") + "ggg") shouldBe StringValue
      ColumnType.determineColTypeValue(Set("12341", "12341.333", "0") + "") shouldBe StringValue

      ColumnType.determineColTypeValue(Set("12341", "-1", "0")) shouldBe IntegerValue
      ColumnType.determineColTypeValue(Set("12341", "+1", "0") + ".1") shouldBe RealValue
      ColumnType.determineColTypeValue(Set("12341", "1", "0") + "") shouldBe StringValue

      ColumnType.determineColTypeValue(Set("True", "true", "FALSE")) shouldBe BooleanValue
      ColumnType.determineColTypeValue(Set("True", "true", "0", "FALSE")) shouldBe StringValue

      ColumnType.determineColTypeValue(Set("12/10/2014", "12/11/2014", "1/10/2014")) shouldBe DateTimeValue
      ColumnType.determineColTypeValue(Set("12/10/2014", "12/11/2014", "July 1st, 1776")) shouldBe StringValue
    }
  }
  "Column category" should {
    val colorData = Seq("red", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue" )
    val numbersTooUnique = Seq("123", "33333", ".432", "444", "55", "66666", "77", "888888", "99", "10000", "333")
    val numericDataNoCategory = Seq("123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123", "123")
    val colorDataTooUnique = Seq("red", "green", "blue", "blue", "blue")
    val stateData = Seq("ME", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA")
    val badState = Seq("QQ", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA")

    "Identify if a category" in {
      ColumnCategory.isCategory( Column(colorData, colorData.toSet)) shouldBe true
      ColumnCategory.isCategory( Column(stateData, stateData.toSet)) shouldBe true
      ColumnCategory.isCategory( Column(numbersTooUnique, colorData.toSet)) shouldBe false
      ColumnCategory.isCategory( Column(Seq(), Set())) shouldBe false
      ColumnCategory.isCategory( Column(colorDataTooUnique, colorDataTooUnique.toSet)) shouldBe false
    }
    "Identify if known category" in {
      ColumnCategory.isKnownCategory(colorData.toSet, ColumnCategory.colors) shouldBe true
      ColumnCategory.isKnownCategory(stateData.toSet, ColumnCategory.stateAbbreviations) shouldBe true
      ColumnCategory.isKnownCategory(badState.toSet, ColumnCategory.stateAbbreviations) shouldBe false
      ColumnCategory.isKnownCategory(Set(), "") shouldBe false
    }
    "Determine category type option" in {
      //Supported categories
      ColumnCategory.determineCategory(Column(colorData, colorData.toSet)) shouldBe Some(Colors)
      ColumnCategory.determineCategory(Column(stateData, stateData.toSet)) shouldBe Some(States)
      //Categories aren't supported
      ColumnCategory.determineCategory(Column(badState, badState.toSet)) shouldBe Some(UnknownCategory)
      ColumnCategory.determineCategory(Column(numericDataNoCategory, numericDataNoCategory.toSet)) shouldBe Some(UnknownCategory)
      //Too unique to be categories
      ColumnCategory.determineCategory(Column(colorDataTooUnique, colorDataTooUnique.toSet)) shouldBe None
      ColumnCategory.determineCategory(Column(numbersTooUnique, numbersTooUnique.toSet)) shouldBe None
    }
  }
}







