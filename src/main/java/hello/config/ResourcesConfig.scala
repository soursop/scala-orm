package hello.config
import com.typesafe.config.{ ConfigValueFactory, Config, ConfigFactory }
import scala.slick.driver.MySQLDriver.api._
import scala.collection.concurrent.Map
import collection.JavaConversions._
import org.apache.spark._
import java.util.concurrent.ConcurrentHashMap
import java.sql.{ PreparedStatement, Connection, ResultSet, DriverManager }


object ResourcesConfig {
  private val profile = sys.props.get("profiles.active").getOrElse("dev")
  private val domain = sys.props.get("domains.active").getOrElse("live")
  private val locale = sys.props.get("locale").getOrElse("kr")
  val ref = ConfigFactory.load()
  val localeRef = ConfigFactory.load("lang_" + locale)
  val defaults = ref.getObject("defaults").toConfig
  val registry = new ConcurrentHashMap[String, Config]()

  def getConfig(): Config = getConfig(profile)
  def getConfig(name: String): Config = {
    registry.putIfAbsent(name, ref.getObject(name).toConfig)
    registry.getOrElse(name, null)
  }
  
  def db: Database = db(profile)
  def db(name: String): Database = {
    val conn = getConfig(name).getObject("conn").toConfig
    Database.forURL(url = conn.getString("url"), driver = conn.getString("driver"), user = conn.getString("user"), password = conn.getString("password"))
  }

  def getConnection: Connection = getConnection(profile)
  def getConnection(name: String): Connection = {
    val conn = getConfig(name).getObject("conn").toConfig
    Class.forName(conn.getString("driver")).newInstance
    DriverManager.getConnection(conn.getString("url"), conn.getString("user"), conn.getString("password"))
  }

}