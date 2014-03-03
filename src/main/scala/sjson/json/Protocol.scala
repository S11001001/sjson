package sjson
package json

import org.json4s.JsonAST._

trait Writes[T] {
  def writes(o: T): JValue
}

trait Reads[T] {
  def reads(json: JValue): T
}

trait Format[T] extends Writes[T] with Reads[T]

trait Protocol {
  implicit val IntFormat: Format[Int]
  implicit val ShortFormat: Format[Short]
  implicit val LongFormat: Format[Long]
  implicit val BooleanFormat: Format[Boolean]
  implicit val FloatFormat: Format[Float]
  implicit val DoubleFormat: Format[Double]
  implicit val StringFormat: Format[String]
}

trait DefaultProtocol extends CollectionTypes with Generic with Primitives
object DefaultProtocol extends DefaultProtocol
