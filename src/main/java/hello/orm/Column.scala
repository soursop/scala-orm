package hello.orm
import scala.reflect.runtime.universe._

case class Column[C: TypeTag](name: String, dbType: String, colNameWithQuote: String) extends Serializable {
  val colName = colNameWithQuote.replaceAll("`", "")
  def typeTag(): Type = typeOf[C]
}