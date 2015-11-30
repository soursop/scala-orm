package hello.orm
import scala.slick.driver.MySQLDriver.api._
import scala.reflect.runtime.universe._
import slick.jdbc.GetResult
import slick.profile.SqlStreamingAction
import scala.concurrent.ExecutionContext.Implicits.global

trait SqlMapper extends Serializable {
  def tableName: String
  val _select = "SELECT %s"
  val _from = f" FROM ${tableName}"
  val _limit = " LIMIT %s"
  val _pagenable = Pagenable(0, 1000)

  def count(): String = {
    _select.format("count(*) as cnt") + _from
  }
  def select(): String = {
    select(_pagenable)
  }
  def select(xs: Seq[Column[_]]): String = {
    select(xs, _pagenable)
  }
  def select(page: Pagenable): String = {
    select(Seq(Column[String]("", "", "*")), page)
  }
  def select(xs: Seq[Column[_]], page: Pagenable): String = {
    _select.format(xs.map(_.colNameWithQuote).mkString(",")) + _from + _limit.format(page.toString)
  }
  
  private def _all = for (
    m <- this.getClass.getDeclaredMethods
      .filter(_.getReturnType.equals(classOf[Column[AnyRef]]))
  ) yield m.getGenericReturnType.getClass match {
    case t if t.equals(classOf[Column[String]]) => m.invoke(this).asInstanceOf[Column[String]]
    case t if t.equals(classOf[Column[Int]])    => m.invoke(this).asInstanceOf[Column[Int]]
    case t if t.equals(classOf[Column[Long]])   => m.invoke(this).asInstanceOf[Column[Long]]
    case _                                      => m.invoke(this).asInstanceOf[Column[Object]]
  }
  
  def all: Seq[Column[_]] = _all.toSeq

}

trait SlickSqlMapper extends SqlMapper {
  def count[R](implicit rconv: GetResult[R]): SqlStreamingAction[Vector[Any], Any, Effect] = {
    sql"#${super.count()}".as[R]
  }
}
