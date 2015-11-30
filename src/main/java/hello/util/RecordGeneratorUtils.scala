package hello.util

import scala.slick.driver.MySQLDriver.api._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import com.google.common.base.CaseFormat
import hello.config.ResourcesConfig
import java.io._
import java.nio.file._

/**
 * @author ncsoft
 */

object RecordGeneratorUtils {
  def main(args: Array[String]): Unit = {

    val generator = RecordGenerator(ResourcesConfig.db("system"))
    val outputPath = Paths.get(ResourcesConfig.getConfig("oplocal").getString("output"));
    val packageRoot = "com.plaync.powerbook.migrator.domain"
    val ormRoot = "com.plaync.powerbook.migrator.orm"
    val classManager = ClassManager("test", "record_effect", outputPath, packageRoot, ormRoot)
    val result = generator.getAll(classManager)
    classManager.generate(result)
  }
}

case class ClassManager(tableSchema: String, tableName: String, ouputPath: Path, packageRoot: String, ormRoot: String) {
  val className = Formatter.underscoreToUpperCamel(tableName.replaceAll("-", "_"))
  val slickDeclared = "import scala.concurrent.ExecutionContext.Implicits.global\nimport scala.slick.driver.MySQLDriver.api._\n"
  val declared = f"package ${packageRoot}\n"
  val ormDeclard = f"import ${ormRoot}._\n"
  def generate(columns: Seq[Column]): Boolean = {
//    extract(className, getClass(columns))
    extract(className + "Mapper", getQueryClass(columns))
    extract(className, getCaseClass(columns))
//    extract(className + "s", getSlickTable(columns))
    true
  }
  def extract(name: String, output: String) = {
    val file = ouputPath.resolve(name + ".scala").toFile()
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(output)
    bw.close()
  }
  def getCaseClass(columns: Seq[Column]): String = {
    val attributesOfClass = getAttributes(columns)
    val bodyOfClass = f"case class ${className} (${attributesOfClass.mkString(", ")}) extends Serializable"
    declared + bodyOfClass
  }
  def getClass(columns: Seq[Column]): String = {
    val suffixOfInput = "Attr"
    val attributesOfClass = getAttributes(columns, suffixOfInput)
    val defsOfClass = columns.map { col => f"def ${col.objectName} = ${col.objectName}${suffixOfInput}" }
    val bodyOfClass = f"class ${className} (${attributesOfClass.mkString(", ")}) extends Serializable {\n  ${defsOfClass.mkString("\n  ")}\n}"
    declared + ormDeclard + bodyOfClass
  }
  def getAttributes(columns: Seq[Column], suffixOfInput: String = ""): Seq[String] = {
    columns.map { col =>
      val attribute = f"${col.objectName}${suffixOfInput}: ${col.objectType}"
      col.objectType match {
        case "Int"  => attribute + " = 0"
        case "Long" => attribute + " = 0"
        case _      => attribute + " = null"
      }
    }
  }
  def getSlickTable(columns: Seq[Column]): String = {
    val headOfClass = f"class ${className}s(tag: Tag) extends Table[${className}](tag, ${"\"" + tableName + "\""})"
    val defsOfClass = columns.map { col => f"  def ${col.objectName} = column[${col.objectType}](${"\"" + col.columnName + "\""})" }
    val cols = columns.map { _.objectName }
    val mapper = f"  def * = (${cols.mkString(",")}) <> (${className}.tupled, ${className}.unapply)"
    f"${declared}${slickDeclared}${headOfClass} {\n${defsOfClass.mkString("\n")}\n${mapper}\n}"
  }
  def getQueryClass(columns: Seq[Column]): String = {
    val defsOfClass = columns.map { col => f"def ${col.objectName} = Column[${col.objectType}](${"\"" + col.objectName + "\""}, ${"\"" + col.columnType + "\""}, ${"\"`" + col.columnName + "`\""})" }
    val queryClass = f"""object ${className}Mapper extends SlickSqlMapper {
    def tableName = \"`${tableName}`\"
    ${defsOfClass.mkString("\n  ")}\n
    }"""
    declared + ormDeclard + queryClass
  }
}

case class RecordGenerator(db: Database) {
  val columns = TableQuery[Columns]

  def getAll(classManager: ClassManager): Seq[Column] = {
    return scala.concurrent.Await.result(
      db.run(
        columns.filter(_.tableSchema === classManager.tableSchema)
          .filter(_.tableNameOfColumn === classManager.tableName)
          .result), scala.concurrent.duration.Duration.Inf)
  }
}

case class Column(val tableName: String, val tableSchema: String, val columnType: String, val columnName: String) {
  val objectType = columnType match {
    case "int(11)"          => "Int"
    case "int(10) unsigned" => "Long"
    case "tinyint(4)"       => "Int"
    case "smallint(6)"      => "Int"
    case _                  => "String"
  }
  val objectName = Formatter.underscoreToCamel(columnName.replaceAll("-", "_")) match {
    case "type" => "typeAttr"
    case x      => x
  }
}

class Columns(tag: Tag) extends Table[Column](tag, "columns") {
  def tableNameOfColumn = column[String]("table_name")
  def tableSchema = column[String]("table_schema")
  def columnType = column[String]("column_type")
  def columnName = column[String]("column_name")
  def * = (tableNameOfColumn, tableSchema, columnType, columnName) <> (Column.tupled, Column.unapply)
}