package sandbox
import shapeless._
import poly._
import syntax.singleton._
import record._

object PolyApp extends App{

  case class Foo(a: Int, b: Int)

  val fooGen = Generic[Foo]

  object size extends Poly1 {
    implicit def caseInt = at[Int](x => 1)
    implicit def caseString = at[String](_.length)
    implicit def caseTuple[T, U]
      (implicit st : Case.Aux[T, Int], su : Case.Aux[U, Int]) =
      at[(T, U)](t => size(t._1)+size(t._2))
  }

  object printL extends Poly1 {
    implicit def caseInt = at[Int](x => println("int: " +1))
    implicit def caseString = at[String](println)
  }


  val l = 23 :: "foo" :: HNil

  val rec = l map printL
//  val foo = fooGen.from(rec)
  println(rec)
}
