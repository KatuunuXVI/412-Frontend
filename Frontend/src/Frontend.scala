import scala.io.Source
import scala.collection
import scala.collection.mutable
import scala.collection.immutable.List
import scala.util.control._

//import Scanner
object Frontend {
  val ILOCfunctions: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop");
  val AcceptingTokens: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//",",","=>");
  var successfulScan: Boolean = true
  //val operations: List[Operation] = List.empty
  val scanner: ILOCScanner = new ILOCScanner
  def main(args: Array[String]): Unit = {
    for(arg <- args) {
      val filename = arg
      var lineNumber: Int = 1
      val input = Source.fromFile(filename)
      for (line <- input.getLines.map(trimWhiteSpace)) {
          //parseLine(line,lineNumber)
        val scan = scanner.scanLine(line,lineNumber)
        print("Line " + lineNumber + " - ")
        if(scan._2) println("Error: " + scan._1.reverse) else println("Success: " + scan._1.reverse)
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

  def scanWord(line: String): (String, String, Boolean) = {
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

  def parseLine(line: String, lineNumber: Int): Unit = {
    var parse: String = line
    var comment: Boolean = false
    val operationParse = scanWord(line)
    val operation: String = operationParse._1
    parse = operationParse._2
    comment = operationParse._3
    if(comment) {
      if(operation.isEmpty) {
        return //Option.empty
      } else {
        if(isValidOperation(operation)) System.err.println("Line " + lineNumber + ": Missing Parameters for " + operation) else System.err.println("Line " + lineNumber + ": " + operation + " is not a valid word")
        return //Option.empty
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

  class ILOCScanner {
    val AcceptedTokens: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//",",","=>")
    def ValidRegisterLabel(register: String): Boolean = {
      if(register.length == 1) {
        register(0) == 'r'
      } else {
        register.length > 1 && register(0) == 'r' && register.substring(1).filterNot(_.isDigit).isEmpty
      }
    }
    def ValidConstant(constant: String): Boolean = !constant.isEmpty && constant.filterNot(_.isDigit).isEmpty
    def scanLine(line: String, lineNumber: Integer): (List[String], Boolean) = {
      var index: Integer = 0
      var tokenIndex: Integer = 0
      var errorState: Boolean = false
      var token: String = ""
      var tokenList: List[String] = List.empty
      var comment: Boolean = false
      var PotentialTokens: mutable.Set[String] = AcceptedTokens
      def c: Char = if(index >= line.length) line.last else line(index)
      while(index < line.length && c.isWhitespace) {
        index += 1
      }
      while(!errorState && index < line.length && !comment) {
        if(!c.isWhitespace) PotentialTokens = PotentialTokens.filter(t => (tokenIndex < t.length) && (t(tokenIndex) == c))
        //println("SCANNING: Line " + lineNumber + ", Index: " + index + ", Token Index: " + tokenIndex + ", Token: " + token + ", Char: " + c + ", Tokens: " + tokenList)
        //println("Potential Tokens: " + PotentialTokens)
        if(!ValidRegisterLabel(token + c) && !ValidConstant(token + c) && PotentialTokens.isEmpty) {
          if(ValidRegisterLabel(token) || AcceptedTokens.contains(token) || ValidConstant(token)) {
            tokenList = tokenList.::(token)
            if(token == "//") comment = true
            token = ""
            tokenIndex = 0
            PotentialTokens = AcceptedTokens
          }
          else {
            token += c
            tokenList = tokenList.::(token)
            errorState = true
          }
        } else {
          //println("SCAN: Added Character/Incremented Index")
          if(!c.isWhitespace) {token += c; tokenIndex += 1}
          index += 1
        }
      }
      if(!errorState && (ValidRegisterLabel(token) || AcceptedTokens.contains(token) || ValidConstant(token)) && token != "") tokenList = tokenList.::(token)
      (tokenList, errorState)
    }
  }


}



