package sjson
package json

import org.json4s.JsonAST._

trait Primitives extends Protocol {
  implicit object IntFormat extends Format[Int] {
    def writes(o: Int) = JInt(o)
    def reads(json: JValue) = json match {
      case JDouble(n) => n.intValue
      case JDecimal(n) => n.intValue
      case JInt(n) => n.intValue
      case _ => throw new RuntimeException("Int expected")
    }
  }

  implicit object ShortFormat extends Format[Short] {
    def writes(o: Short) = JInt(o)
    def reads(json: JValue) = json match {
      case JDouble(n) => n.shortValue
      case JDecimal(n) => n.shortValue
      case JInt(n) => n.shortValue
      case _ => throw new RuntimeException("Short expected")
    }
  }

  implicit object LongFormat extends Format[Long] {
    def writes(o: Long) = JInt(o)
    def reads(json: JValue) = json match {
      case JDouble(n) => n.longValue
      case JDecimal(n) => n.longValue
      case JInt(n) => n.longValue
      case _ => throw new RuntimeException("Long expected")
    }
  }

  implicit object FloatFormat extends Format[Float] {
    def writes(o: Float) = JDouble(o)
    def reads(json: JValue) = json match {
      case JDouble(n) => n.floatValue
      case JDecimal(n) => n.floatValue
      case JInt(n) => n.floatValue
      case _ => throw new RuntimeException("Float expected")
    }
  }

  implicit object DoubleFormat extends Format[Double] {
    def writes(o: Double) = JDouble(o)
    def reads(json: JValue) = json match {
      case JDouble(n) => n
      case JDecimal(n) => n.doubleValue
      case JInt(n) => n.doubleValue
      case _ => throw new RuntimeException("Double expected")
    }
  }

  implicit object BooleanFormat extends Format[Boolean] {
    def writes(o: Boolean) = JBool(o)
    def reads(json: JValue) = json match {
      case JBool(b) => b
      case _ => throw new RuntimeException("Boolean expected")
    }
  }

  implicit object StringFormat extends Format[String] {
    def writes(o: String) = JString(o)
    def reads(json: JValue) = json match {
      case JString(s) => s
      case _ => throw new RuntimeException("String expected")
    }
  }
  implicit object JsValueFormat extends Format[JValue] {
    def writes(o: JValue) = o
    def reads(json: JValue) = json
  }
}
