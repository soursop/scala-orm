package hello.config
import org.apache.spark._

object SparkConfig {
  
  lazy val sc = _sparkContext()

  private def _sparkContext() = {
    val conf = new SparkConf().setAppName("Test spark")
    new SparkContext(conf)
  }
  
}