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
    if(line.isEmpty) return
    var index: Integer = 0
    val functionCall: String = trimWhiteSpace(line)
    if(functionCall.isEmpty) return
    var potentFuncs = ILOCfunctions
    //val loopBreak: Breaks = new Breaks;
    while(potentFuncs.size > 0 && functionCall(index) != ' ') {
      potentFuncs = potentFuncs.filter(f => index < f.length && f(index) == functionCall(index))
      println(functionCall(index) + "-> Potential Functions: " + potentFuncs)
      index += 1
    }
    if(potentFuncs.isEmpty) {
      println("Line " + lineNumber + ": " + functionCall.substring(0,index) + " is not a valid word")
    }
    if(potentFuncs.toList(0) == "//") {
      return
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
