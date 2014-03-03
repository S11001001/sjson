package sjson
package json

import DefaultProtocol._

object Protocols {
  case class Person(lastName: String, firstName: String, age: Int)
  object Person extends DefaultProtocol {
    import org.json4s.JsonAST._
    import JsonSerialization._
    implicit object PersonFormat extends Format[Person] {
      def reads(json: JValue): Person = json match {
        case JObject(m) =>
          Person(fromjson[String](m(JString("lastName"))), 
            fromjson[String](m(JString("firstName"))), fromjson[Int](m(JString("age"))))
        case _ => throw new RuntimeException("JObject expected")
      }
      def writes(p: Person): JValue =
        JObject(List(
          (tojson("lastName").asInstanceOf[JString], tojson(p.lastName)), 
          (tojson("firstName").asInstanceOf[JString], tojson(p.firstName)), 
          (tojson("age").asInstanceOf[JString], tojson(p.age)) ))
    }
  }

  case class Shop(store: String, item: String, price: Int)
  implicit val ShopFormat: Format[Shop] = 
    asProduct3("store", "item", "price")(Shop)(Shop.unapply(_).get)

  case class Address(street: String, city: String, zip: String)
  implicit val AddressFormat: Format[Address] = 
    asProduct3("street", "city", "zip")(Address)(Address.unapply(_).get)

  case class Contact(name: String, addresses: List[Address])
  implicit val ContactFormat: Format[Contact] = 
    asProduct2("name", "addresses")(Contact)(Contact.unapply(_).get)

  case class Account(no: String, name: String, addresses: Array[Address])
  implicit val AccountFormat: Format[Account] = 
    asProduct3("no", "name", "addresses")(Account)(Account.unapply(_).get)

  case class Base(no: String, name: String, addresses: Array[Address])
  implicit val BaseFormat: Format[Base] = 
    asProduct3("no", "name", "addresses")(Base)(Base.unapply(_).get)

  class Derived(no: String, name: String, addresses: Array[Address], special: Boolean) 
    extends Base(no, name, addresses) {
    val specialFlag = special
  }
  object Derived extends DefaultProtocol {
    import org.json4s.JsonAST._
    import JsonSerialization._
    implicit object DerivedFormat extends Format[Derived] {
      def reads(json: JValue): Derived = {
        val b = fromjson[Base](json)
        json match {
          case JObject(m) =>
            new Derived(b.no, b.name, b.addresses,
              fromjson[Boolean](m(JString("specialFlag"))))
          case _ => throw new RuntimeException("JObject expected")
        }
      }
      def writes(a: Derived): JValue = {
        val o = tojson(a: Base)
        val JObject(m) = o
        JObject(m ++ List((tojson("specialFlag").asInstanceOf[JString], tojson(a.specialFlag))))
      }
    }
  }

  case class Name(name: String)
  implicit val NameFormat: Format[Name] = wrap[Name, String]("name")(_.name, Name)

  case class Holder(item: List[String])
  implicit val HolderFormat: Format[Holder] = wrap[Holder, List[String]]("item")(_.item, Holder)

  case class DoubleNanTest(price: Double)
  import org.json4s.JsonAST._
  implicit val DoubleNanTestFormat: Format[DoubleNanTest] = new Format[DoubleNanTest] {
    def reads(json: JValue): DoubleNanTest = json match {
      case JString("Double.NaN") => DoubleNanTest(scala.Double.NaN)
      case JsNumber(n) => DoubleNanTest(n.doubleValue)
      case _ => sys.error("Invalid DoubleNanTest")
    }
    def writes(a: DoubleNanTest): JValue = a.price match {
      case x if x equals scala.Double.NaN => JString("Double.NaN")
      case x => JsNumber(BigDecimal.valueOf(x))
    }
  }

  import TestBeans._
  implicit val AddressWithOptionalCityFormat: Format[AddressWithOptionalCity] =
    asProduct3("street", "city", "zip")(AddressWithOptionalCity)(AddressWithOptionalCity.unapply(_).get)

  // example for inheritance and case objects
  import org.json4s.JsonAST._
  trait HttpType
  implicit val HttpTypeFormat: Format[HttpType] = new Format[HttpType] {
    def reads(json: JValue): HttpType = json match {
      case JString("Get") => Get
      case JString("Post") => Post
      case _ => sys.error("Invalid HttpType")
    }
    def writes(a: HttpType): JValue = a match {
      case Get => JString("Get")
      case Post => JString("Post")
    }
  }

  case object Get extends HttpType
  case object Post extends HttpType

  case class Http(url: String, t: HttpType)
  implicit val HttpFormat: Format[Http] = 
    asProduct2("url", "t")(Http)(Http.unapply(_).get)

  case class Bar(name: String, list: Option[List[Foo]])
  case class Foo(name: String, list: List[Bar])
  implicit val BarFormat: Format[Bar] = lazyFormat(asProduct2("name", "list")(Bar)(Bar.unapply(_).get))
  implicit val FooFormat: Format[Foo] = lazyFormat(asProduct2("name", "list")(Foo)(Foo.unapply(_).get))

  case class JobStart(name: String, start: WeekDay.Value)
  object JobStart extends DefaultProtocol {
    import JsonSerialization._
    implicit object JobStartFormat extends Format[JobStart] {
      def reads(json: JValue): JobStart = json match {
        case JObject(m) =>
          JobStart(fromjson[String](m(JString("name"))), 
            WeekDay.withName(fromjson[String](m(JString("start")))))
        case _ => throw new RuntimeException("JObject expected")
      }
      def writes(p: JobStart): JValue =
        JObject(List(
          (tojson("name").asInstanceOf[JString], tojson(p.name)), 
          (tojson("start").asInstanceOf[JString], tojson(p.start.toString)))) 
    }
  }

  // the following example has 2 aspects :-
  // 1. Inheritance of traits
  // 2. Recursive types

  trait SubUnit
  case class Dept(name: String, manager: Employee, subUnits: List[SubUnit]) extends SubUnit
  case class Employee(name: String, salary: Double) extends SubUnit

  object SubUnit extends DefaultProtocol {
    import JsonSerialization._
    implicit object SubUnitFormat extends Format[SubUnit] {
      def reads(json: JValue): SubUnit = json match {
        case j@JObject(m) => m.keys.size match {
          case 2 => fromjson[Employee](j)
          case _ => fromjson[Dept](j)
        }

        case _ => throw new RuntimeException("JObject expected")
      }

      def writes(s: SubUnit): JValue = s match {
        case d: Dept => tojson(d)
        case e: Employee => tojson(e)
      }
    }
  }

  // import SubUnitProtocol._
  implicit val DeptFormat: Format[Dept] = 
    lazyFormat(asProduct3("name", "manager", "subUnits")(Dept)(Dept.unapply(_).get))

  implicit val EmployeeFormat: Format[Employee] = 
    asProduct2("name", "salary")(Employee)(Employee.unapply(_).get)

  case class P(lastName: String, firstName: String, age: Option[Int] = None)
  object P extends DefaultProtocol {
    import org.json4s.JsonAST._
    import JsonSerialization._
    implicit object PFormat extends Format[P] {
      def reads(json: JValue): P = json match {
        case JObject(m) =>
          P(
            fromjson[String](m(JString("lastName"))), 
            fromjson[String](m(JString("firstName"))), 
            m.get(JString("age")).map(fromjson[Option[Int]](_)).getOrElse(None))
        case _ => throw new RuntimeException("JObject expected")
      }
      def writes(p: P): JValue =
        p.age.map(a =>
          JObject(List(
            (tojson("lastName").asInstanceOf[JString], tojson(p.lastName)),
            (tojson("firstName").asInstanceOf[JString], tojson(p.firstName)),
            (tojson("age").asInstanceOf[JString], tojson(a)))))
        .getOrElse(
          JObject(List(
            (tojson("lastName").asInstanceOf[JString], tojson(p.lastName)),
            (tojson("firstName").asInstanceOf[JString], tojson(p.firstName)))))
    }
  }

  // Issue #37 (https://github.com/debasishg/sjson/issues/37)
  case class User(val id : scala.Option[scala.Predef.String], 
                  val username : scala.Predef.String, 
                  val org : scala.Predef.String, 
                  val firstname : scala.Predef.String, 
                  val lastname : scala.Predef.String)

  case class DataGridResult (totalCount: String, success: Boolean, results: Seq[User])

  object DataGridResultProtocol extends DefaultProtocol {
    import org.json4s.JsonAST._
    import JsonSerialization._

    implicit val UserFormat: Format[User] =
      asProduct5("id", "username", "org", "firstname", "lastname")(User)(User.unapply(_).get)

    implicit val DataGridFormat: Format[DataGridResult] =
      asProduct3("totalCount", "success", "results")(DataGridResult)(DataGridResult.unapply(_).get)  
  }

  abstract class C1 {
    type T
    val v: T
  }

  case class CC1(v: String) extends C1 {
    type T = String
  }

  object CC1 extends DefaultProtocol {
    import org.json4s.JsonAST._
    import JsonSerialization._

    implicit object CC1Format extends Format[CC1] {
      def reads(json: JValue): CC1 = json match {
        case JObject(m) => CC1(fromjson[String](m(JString("v")))) 
        case _ => throw new RuntimeException("JObject expected")
      }
      def writes(c1: CC1): JValue = JObject(List((tojson("v").asInstanceOf[JString], tojson(c1.v))))
    }
  }
}
