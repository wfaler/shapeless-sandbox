package jdbc.shapeless

import shapeless._
import poly._
import java.sql.{PreparedStatement,Connection}

object DB extends App{
  case class StatementAccumulator(statement: PreparedStatement, index: Int = 1)

  case class Table[A](name: String, fields: List[String]){
    def insert = {
      val fieldsStatement = fields.mkString(",")
      val placeholders = (for(i <- 1 to fields.length) yield "?").toList.mkString(",")
      s"INSERT INTO $name ($fieldsStatement) VALUES ($placeholders)"
    }
  }

  object setField extends Poly1 {
    implicit def caseString = at[String]{ value => (stmt: PreparedStatement, index: Int) => stmt.setString(index, value)}
  }

  object toRow extends Poly2 {
    implicit def statementFn = at[StatementAccumulator, (PreparedStatement, Int) => Unit]{ (acc, fn) => 
      fn(acc.statement, acc.index)
      StatementAccumulator(acc.statement, acc.index + 1)
    }
  }


  // foo specific code
  case class Foo(s: String, b: String)
  val gen = Generic[Foo]
  val table = Table[Foo]("foo_table", "s" :: "b" :: Nil)

  def insert(foo: Foo, conn: Connection) = {
    val statement = conn.prepareStatement(table.insert)
    val result = (gen.to(Foo("foo","Bar")) map setField).foldLeft(StatementAccumulator(statement))(toRow)
    statement.executeUpdate == 1
  }
  

    
    
 
  

}

