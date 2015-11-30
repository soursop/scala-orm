package hello.orm

case class Pagenable(start: Long, limit: Long, enableQuestion: Boolean=false) {
  def this() = this(0, 0, true)
  override def toString(): String = enableQuestion match {
    case false => "%d, %d".format(start, limit)
    case true  => "%s, %s".format("?", "?")
  }
  def next: Pagenable = {
    Pagenable(start + limit, limit)
  }
  def hasNext(total: Long): Boolean = {
    val remain = total - (start + limit)
    if (remain >= 0) true
    else remain + limit > 0
  }
}
