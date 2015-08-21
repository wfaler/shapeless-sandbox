package jdbc.shapeless

import shapeless._
import poly._
import java.sql.{PreparedStatement}

object DB extends App{
  case class Foo(s: String, b: String)
  case class StatementAccumulator(statement: PreparedStatement, index: Int = 1)

  object setField extends Poly1 {
    implicit def caseString = at[String]{ value => (stmt: PreparedStatement, index: Int) => stmt.setString(index, value)}
  }

  object toRow extends Poly2 {
    implicit def statementFn = at[StatementAccumulator, (PreparedStatement, Int) => Unit]{ (acc, fn) => 
      fn(acc.statement, acc.index)
      StatementAccumulator(acc.statement, acc.index + 1)
    }
  }

  def toFields[A](product: A, statement: PreparedStatement)(implicit gen: Generic[A]) = {
    def toFields2[A](product: A)(implicit gen: Generic[A]) = {
      val g = Generic[A]
      val hl = g.to(product)
      hl
      //  val result = hl map setField
      //    result.foldLeft(StatementAccumulator(statement))(toRow)
    }
    val list = toFields2(product)
    list..map(setField)
  }
  

//  println(toFields(Foo("s","b"),null).map(setField))


}

