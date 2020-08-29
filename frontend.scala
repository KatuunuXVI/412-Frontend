import scala.io.Source
import scala.collection.mutable
import scala.util.control._
object Frontend {
  val ILOCfunctions: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//");
  def main(args: Array[String]): Unit = {
    println(ILOCfunctions)
    for(arg <- args) {
      val filename = arg
      var lineNumber: Int = 0
      for (line <- Source.fromFile("inputs/parse.i").getLines) {
        parseLine(line,lineNumber)
        lineNumber += 1
      }
    }
  }

  def parseLine(line: String, lineNumber: Int): Unit = {
    println(line)
    def valid: Boolean = true

    var index: Integer = 0
    val functionCall: String = trimWhiteSpace(line)
    var potentFuncs = ILOCfunctions
    //val loopBreak: Breaks = new Breaks;
    while(potentFuncs.size > 1) {
      potentFuncs = potentFuncs.filter(_(index) == functionCall(index))
      index += 1
    }
    if(potentFuncs.isEmpty) {
      println("Line " + lineNumber + ": " + functionCall.substring(0,index) + " is not a valid word")
    }
  }

  def trimWhiteSpace(line: String): String = {
    var index: Int = 0
    while(line(index) == ' ') {
      index += 1
    }
    line.substring(index)
  }

}
