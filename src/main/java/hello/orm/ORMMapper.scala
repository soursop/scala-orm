package hello.orm
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import java.sql.ResultSet

object ORMMapper {
  
  def reflectConstructor[T](implicit tag: TypeTag[T]): MethodMirror = synchronized {
    val targetType = tag.tpe
    val classMirror = currentMirror.reflectClass(targetType.typeSymbol.asClass)
    val ctor = targetType.declaration(nme.CONSTRUCTOR).asMethod
    classMirror.reflectConstructor(ctor)
  }
  
  def getParameters[T](constructor: MethodMirror)(implicit cols: Seq[Column[_]]): Seq[Any] = {
    val params = constructor.symbol.paramss.head
    val namesOfcols = cols.map(_.colNameWithQuote.replaceAll("`", ""))
    val indexesFields = cols.zipWithIndex.map {
      c => params.find { x => x.name.toString.equals(c._2.name) } match {
        case Some(_) => (c._1.name, c._2 + 1, c._1.typeTag, true)
        case _ => (c._1.colNameWithQuote, c._2 + 1, NoType, false)
      }
    }
    val (exist, none) = indexesFields.partition(_._4)
    val parameters = (Seq[Any]() /: constructor.symbol.paramss.head)((acc: Seq[_], x) =>
      exist.find(y => y._1.equals(x.name.toString)) match {
        case Some(x) => acc :+ (x._2, x._3)
        case _ => x.typeSignature match {
          case t if t =:= typeOf[Int]       => acc :+ 0
          case t if t =:= typeOf[Long]      => acc :+ 0
          case t if t =:= typeOf[String]    => acc :+ null
          case t if t <:< typeOf[Map[_,_]] => acc :+ none.map(x=>x._1->(x._2, x._3)).toMap
          case _                            => acc :+ null
        }
      })
    parameters
  }
  
  def getParameters[T](implicit tag: TypeTag[T], cols: Seq[Column[_]]): Seq[Any] = {
    val constructor = reflectConstructor[T](tag)
    getParameters(constructor)
  }
  
  private def getValue(i: Int, t: Type, r: ResultSet): Any = t match {
    case t if t =:= typeOf[Int]    => r.getInt(i)
    case t if t =:= typeOf[Long]   => r.getLong(i)
    case t if t =:= typeOf[String] => r.getString(i)
    case _                         => r.getObject(i)
  }
    
  def getResult[T](r: ResultSet, constructor: MethodMirror)(implicit params: Seq[Any]): T = {
    val parameters = params.map {
      x => x match {
        case null => null
        case 0 => 0
        case (idx: Int, t: Type) => getValue(idx, t, r)
        case x: Map[_, _] => x.asInstanceOf[Map[String, (Int, Type)]].map {
          y => y._1 -> getValue(y._2._1, y._2._2, r)
        }.filter(_._2 != null)
      }
    }
    constructor.apply(parameters : _*).asInstanceOf[T]
  }
  
  def getResult[T](r: ResultSet)(implicit tag: TypeTag[T], params: Seq[Any]): T = {
    val constructor = reflectConstructor
    getResult(r, constructor)
  }
}