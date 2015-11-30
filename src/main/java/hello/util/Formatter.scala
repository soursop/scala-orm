package hello.util

object Formatter {
  
  def camelToUnderscores(name: String) = "[A-Z\\d]".r.replaceAllIn(name, { m =>
    "_" + m.group(0).toLowerCase()
  })
  def underscoreToUpperCamel(name: String) = {
    val (first, remain) = underscoreToCamel(name).splitAt(1)
    first.toUpperCase + remain
  }
  def underscoreToCamel(name: String) = "_([a-z\\d])".r.replaceAllIn(name, { m =>
    m.group(1).toUpperCase()
  })
  
  def arrayToVal(str: String) {
    val quetes = str.split(",")
    val vals = quetes.map(_.replaceAll("\"", "")).map(_.replaceAll("-", "_")).map(x=>underscoreToCamel(x))
    (vals zip quetes).foreach{
//      x => printf("val %s = Value(\"%s\")\n", x._1, x._2)
    		  x => printf(",%s\n", x._1)
    }
  }
}