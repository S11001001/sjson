package sjson
package json

import org.json4s.JsonAST._

object JsonSerialization {
  def tojson[T](o: T)(implicit tjs: Writes[T]): JValue = tjs.writes(o)

  def fromjson[T](json: JValue)(implicit fjs: Reads[T]): T = fjs.reads(json)

  def tobinary[T](o: T)(implicit tjs: Writes[T]): Array[Byte] = 
    JValue.toJson(tojson(o)).getBytes("UTF-8")

  def frombinary[T](bytes: Array[Byte])(implicit fjs: Reads[T]): T =
    fromjson(Js(new String(bytes, "UTF-8")))
}
