

import scala.io.Source
import scala.collection.mutable
import scala.util.control._
object Frontend {
  val ILOCfunctions: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop");
  var successfulScan: Boolean = true
  val operations: List[Operation] = List.empty
  def main(args: Array[String]): Unit = {
    for(arg <- args) {
      val filename = arg
      //println(arg)
      var lineNumber: Int = 1
      val input = Source.fromFile(filename)
      for (line <- input.getLines.map(trimWhiteSpace)) {
        if(!line.isEmpty && !isComment(line)) {
          println("Parsing Line " + lineNumber + ": " + line)
          parseLine(line,lineNumber)
        }
        lineNumber += 1
      }
      input.close()
    }

  }

  def isComment(word:String): Boolean = word.substring(0,2) =="//"

  def isValidOperation(word: String): Boolean = ILOCfunctions.contains(word)

  def isValidRegister(word: String): Boolean = {
    val commaHead = word(0) == ','
    val commaTail = word.last == ','
    val regLabel = word(0) == 'r'
    val regSecond = word(1) == 'r'
    val regNumbered = isValidConstant(word.substring(1))
    val regNumberedCH = isValidConstant(word.substring(2,word.length))
    val regNumberedCT = isValidConstant(word.substring(1,word.length-1))
    val regNumberedCHCT  = isValidConstant(word.substring(2, word.length-1))
    (commaHead && regSecond && regNumberedCHCT && commaTail) || /** ,rx, **/
      (regLabel && regNumberedCT && commaTail) || /** rx, **/
      (commaHead && regSecond && regNumberedCH) || /** ,rx **/
      (regLabel && regNumbered) /** rx **/
  }

  def isValidConstant(word: String): Boolean = !word.forall(_.isDigit)

  def parseWord(line: String): (String, String, Boolean) = {
    var potCom: Boolean = false
    var comment: Boolean = false
    var index: Integer = 0
    val builder: String = trimWhiteSpace(line)
    var word: String = ""
    def curChar: Char = builder(index)
    while(!comment && curChar != ' ' && index < builder.length) {
      if(curChar == '/') {
        if(!potCom) {word += curChar; potCom = true} else {comment = true; word = word.substring(0,index-1)}
        index += 1
      } else {
        if(potCom) potCom = false
        word += curChar
        index += 1
      }
    }
    (word, trimWhiteSpace(line.substring(index)), comment)
  }

  def parseLine(line: String, lineNumber: Int): Option[Operation] = {
    var parse: String = line
    var comment: Boolean = false
    val operationParse = parseWord(line)
    val operation: String = operationParse._1
    parse = operationParse._2
    comment = operationParse._3
    if(comment) {
      if(operation.isEmpty) {
        return Option.empty
      } else {
        if(isValidOperation(operation)) System.err.println("Line " + lineNumber + ": Missing Parameters for " + operation) else System.err.println("Line " + lineNumber + ": " + operation + " is not a valid word")
        return Option.empty
      }
    }
    if(isValidOperation(operation)) println("Valid Operator") else System.err.println("Line " + lineNumber + ": " + operation + " is not a valid word")
    Option.empty
  }

  def parseTwoPar(operation: String, line: String, lineNumber: Integer): Option[String] = {
    if (operation == "loadI") {
      var index: Int = 0

      def curChar: Char = line(index)

      var potCom: Boolean = false
      var comment: Boolean = false
      var constant: String = ""
    }
    ???
  }
def parseThreePar(operation: String): Unit = {

  }

  def parseOutput(): Unit = {

  }

  def trimWhiteSpace(line: String): String = {
    if(line.isEmpty) return line
    var index: Int = 0
    while(index < line.length && line(index) == ' ') {
      index += 1
    }
    if(index < line.length) line.substring(index) else ""
  }

  case class Load(register1: String, register2: String)

  abstract class Operation
}
