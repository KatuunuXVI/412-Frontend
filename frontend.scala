import scala.collection.mutable
import scala.collection.immutable.List
import scala.io.Source

//import ILOCScanner
object Frontend {
  val AcceptingTokens: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//",",","=>");
  var successfulScan: Boolean = true
  //val operations: List[Operation] = List.empty
  val scanner: ILOCScanner = new ILOCScanner
  def main(args: Array[String]): Unit = {
    println("Hello World")
    for(arg <- args) {
      val filename = arg
      var lineNumber: Int = 1
      val input = Source.fromFile(filename)
      for (line <- input.getLines) {
          //parseLine(line,lineNumber)
        val scan = scanner.scanLine(line,lineNumber)
        print("Line " + lineNumber + " - ")
        if(scan._2.isDefined) println("Error: " + scan._1.reverse) else println("Success: " + scan._1.reverse)
        lineNumber += 1
      }
      input.close()
    }

  }

  def isComment(word:String): Boolean = word.substring(0,2) =="//"

  class ILOCScanner {
    //val ILOCfunctions: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop");
    val AcceptedTokens: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//",",","=>")
    //def isValidOperation(word: String): Boolean = ILOCfunctions.contains(word)

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

    def ValidRegisterLabel(register: String): Boolean = {
      if(register.length == 1) {
        register(0) == 'r'
      } else {
        register.length > 1 && register(0) == 'r' && register.substring(1).filterNot(_.isDigit).isEmpty
      }
    }

    def ValidConstant(constant: String): Boolean = !constant.isEmpty && constant.filterNot(_.isDigit).isEmpty

    def scanLine(line: String, lineNumber: Integer): (List[String], Option[String]) = {
      var index: Integer = 0
      var tokenIndex: Integer = 0
      var errorState: Boolean = false
      var token: String = ""
      var tokenList: List[String] = List.empty
      var comment: Boolean = false
      var PotentialTokens: mutable.Set[String] = AcceptedTokens
      var errorWord: Option[String] = Option.empty

      def c: Char = if(index >= line.length) line.last else line(index)
      while(index < line.length && c.isWhitespace) {
        index += 1
      }
      while(!errorState && index < line.length && !comment) {
        if(!c.isWhitespace) PotentialTokens = PotentialTokens.filter(t => (tokenIndex < t.length) && (t(tokenIndex) == c))
        if((!ValidRegisterLabel(token + c) && !ValidConstant(token + c) && PotentialTokens.isEmpty) || (c.isWhitespace && !token.isEmpty)) {
          //print("Token: " + token + " - ")
          if(ValidRegisterLabel(token) || AcceptedTokens.contains(token) || ValidConstant(token)) {
            //println("Valid")
            tokenList = tokenList.::(token)
            if(token == "//") comment = true
            token = ""
            tokenIndex = 0
            PotentialTokens = AcceptedTokens
          }
          else {
            //println("Invalid")
            token += c
            tokenList = tokenList.::(token)
            errorWord = Option(token)
            errorState = true
          }
        } else {
          if(!c.isWhitespace) {token += c; tokenIndex += 1}
          index += 1
        }
      }
      if(!errorState && (ValidRegisterLabel(token) || AcceptedTokens.contains(token) || ValidConstant(token)) && token != "") tokenList = tokenList.::(token)
      (tokenList, errorWord)
    }
  }

  class ILOCParser {
    import Frontend._
    def parseLine(line: List[String], lineNumber: Integer): Option[Operation] = {
      val head = line.head
      val tail = line.tail
      val needOp: Integer = 0
      val needReg: Integer = 1
      val needCon: Integer = 2
      val needAssi: Integer = 3
      val needCom: Integer = 4
      head match {
        case "load" => validLoad(tail, lineNumber)
        case "loadI" => validLoadI(tail)
        case "store" => validStore(tail)
        case "add" => validAdd(tail)
        case "sub" => validSub(tail)
        case "mult" => validMult(tail)
        case "lshift" => validLShift(tail)
        case "rshift" => validRShift(tail)
        case "output" => validOutput(tail)
        case "nop" => validNop(tail)
        case _=> {
          println("PARSING ERROR [" + lineNumber + "]: Operation begins with invalid OpCode " + head)
          Option.empty
        }
      }
    }

    def validLoad(tokens: List[String], lineNumber: Integer): Option[Load] = {
      var r1: Option[Register] = Option.empty
      var r2: Option[Register] = Option.empty
      var index = 0
      //while(index)
      ???
    }

    def validLoadI(tokens: List[String]): Option[LoadI] = ???

    def validStore(tokens: List[String]): Option[Store] = ???

    def validAdd(tokens: List[String]): Option[Add] = ???

    def validSub(tokens: List[String]): Option[Sub] = ???

    def validMult(tokens: List[String]): Option[Mult] = ???

    def validLShift(tokens: List[String]): Option[LShift] = ???

    def validRShift(tokens: List[String]): Option[RShift] = ???

    def validOutput(tokens: List[String]): Option[Output] = ???

    def validNop(tokens: List[String]): Option[Nop] = ???
  }

  case class Load(r1: Register, r2: Register) extends Operation {
    override def execute(): Unit = ???
  }

  case class LoadI(x: Integer, r: Register) extends Operation {
    override def execute(): Unit = ???
  }

  case class Store(r1: Register, r2:Register) extends Operation {
    override def execute(): Unit = ???
  }

  case class Add(r1: Register, r2: Register, r3: Register) extends Operation {
    override def execute(): Unit = ???
  }

  case class Sub(r1: Register, r2: Register, r3: Register) extends Operation {
    override def execute(): Unit = ???
  }

  case class Mult(r1: Register, r2: Register, r3: Register) extends Operation {
    override def execute(): Unit = ???
  }

  case class LShift(r1: Register, r2: Register, r3: Register) extends Operation {
    override def execute(): Unit = ???
  }

  case class RShift(r1: Register, r2: Register, r3: Register) extends Operation {
    override def execute(): Unit = ???
  }

  case class Output(x: Integer) extends Operation {
    override def execute(): Unit = ???
  }

  case class Nop() extends Operation {
    override def execute(): Unit = ???
  }

  abstract class Operation {
    def execute(): Unit
  }

  case class Register(value: Option[Integer]) {

  }


}

