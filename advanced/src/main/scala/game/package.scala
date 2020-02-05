package object game {
  val newLine: String = sys.props("line.separator")

  def readInt(): Int = {
    scala.io.StdIn.readInt()
  }

  def readLine(msg: String): String = {
    print(msg)
    scala.io.StdIn.readLine()
  }

  def addNewLine(): Unit = {
    println()
  }

  def log(msg: String): Unit = {
    println(msg)
  }
}
