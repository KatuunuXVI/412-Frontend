import scala.collection.{immutable, mutable}
import scala.collection.immutable.List
import scala.io.Source

//import ILOCScanner
object Frontend {
  val AcceptingTokens: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//",",","=>");
  var successfulScan: Boolean = true

  val scanner: ILOCScanner = new ILOCScanner
  val parser: ILOCParser = new ILOCParser
  var flag: String = "-p"
  var flagPriority = 0
  var filename = ""
  var fileReceived = false
  var errorCount: Int = 0
  def main(args: Array[String]): Unit = {
    for(arg <- args) {
      arg match {
        case "-h" =>
          if(flagPriority < 4) {
            flag = "-h"
            flagPriority = 4
          }
        case "-s" =>
          if(flagPriority < 1) {
            flag = "-s"
            flagPriority = 1
          }
        case "-p" =>
          if(flagPriority < 2) {
            flag = "-p"
            flagPriority = 2
          }
        case "-r" =>
          if(flagPriority < 3) {
            flag = "-r"
            flagPriority = 3
          }
        case _=>
          if(filename.isEmpty) filename = arg else flag = "-h"
      }
    }
    //println("Flag: " + flag)
    flag match {
      case "-h" =>
        println("COMMAND SYNTAX: scala Frontend.scala [flag] <filename>\n")
        println("REQUIRED ARGUMENTS: filename (absolute or relative) to the input\n")
        println("OPTIONAL FLAGS:")
        println("-h: Prints this message")
        println("-r: Prints out Intermediate Representation")
        println("-p: Prints out Parse Status")
        println("-s: Prints out all found tokens")
      case "-r" =>
        val IR = scanAndParse(filename)
        if(errorCount == 0) {
          for(op <- IR.filter(_.isDefined).map(_.get)) {
            println(op)
          }
        } else {
          println("Due to Syntax Errors, Intermediate Representation not Available")
        }
      case "-p" =>
        val IR = scanAndParse(filename)
        if(errorCount == 0) println("Parse Succeeded, " + IR.count(_.isDefined) + " ILOC operations found") else {
          val input = Source.fromFile(filename)
          println("Parser found " + errorCount + " syntax errors in " + input.getLines().length + " lines of input")
          input.close()
        }

      case "-s" =>
        scanAndParse(filename)
    }
  }

  def scanAndParse(filename: String): List[Option[Operation]] = {
    var lineNumber: Int = 1
    var fullScan: List[List[Any]] = List.empty
    val input = Source.fromFile(filename)
    for (line <- input.getLines) {
      //parseLine(line,lineNumber)
      val scan: List[Any] = scanner.scanLine(line,lineNumber).reverse
      //print("Line " + lineNumber + " - ")
      //if(scan.contains(-1)) println("Error: " + scan) else println("Success: " + scan)
      lineNumber += 1
      fullScan = fullScan.::(scan.filterNot(l => l == -1 || l == 10))
    }
    fullScan = fullScan.reverse
    input.close()
    lineNumber = 1
    var interRep: List[Option[Operation]] = List.empty
    for(i <- fullScan) {
      interRep = interRep.::(parser.parse(i,lineNumber))
      lineNumber += 1
    }
    interRep = interRep.reverse
    interRep
  }

  class ILOCScanner {
    //val ILOCfunctions: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop");
    val AcceptedTokens: mutable.Set[String] = mutable.Set("load","loadI","store","add","sub","mult","lshift","rshift","output","nop","//",",","=>")
    //def isValidOperation(word: String): Boolean = ILOCfunctions.contains(word)

    def TokenMap(op: String): Any = {
      op match {
        case "load" => 0
        case "loadI" => 1
        case "store" => 2
        case "add" => 3
        case "sub" => 4
        case "mult" => 5
        case "lshift" => 6
        case "rshift" => 7
        case "output" => 8
        case "nop" => 9
        case "//" => 10
        case "," => 11
        case "=>" => 12
        case " " => 15
        case _=>
          if(ValidRegisterLabel(op, true)) {
            Register(Integer.parseInt(op.substring(1)),Option.empty)
          } else if(ValidConstant(op)) {
            Constant(Integer.parseInt(op))
          } else {
            -1
          }
        }
      }

    def TokenNotation(op: String): Any = {
      op match {
        case "load" | "store" => "MEMOP"
        case "loadI" => "LOADI"
        case "add" | "sub" | "mult" | "lshift" | "rshift" => "ARITHOP"
        case "output" => "OUTPUT"
        case "nop" => "NOP"
        case "//" => "COMMENT"
        case "," => "COMMA"
        case "=>" => "INTO"
        case _=>
          if(ValidRegisterLabel(op, true)) {
            "REG"
          } else if(ValidConstant(op)) {
            "CONST"
          } else {
            -1
          }
      }
    }

    def ValidRegisterLabel(register: String, complete: Boolean): Boolean = {
      if(register.length == 1) {
        if(complete) false else register(0) == 'r'
      } else {
        register.length > 1 && register(0) == 'r' && register.substring(1).filterNot(_.isDigit).isEmpty
      }
    }

    def ValidConstant(constant: String): Boolean = !constant.isEmpty && constant.filterNot(_.isDigit).isEmpty

    def scanLine(line: String, lineNumber: Integer): List[Any] = {
      var index: Integer = 0
      var tokenIndex: Integer = 0
      var errorState: Boolean = false
      var token: String = ""
      var tokenList: List[Any] = List.empty
      var comment: Boolean = false
      var PotentialTokens: mutable.Set[String] = AcceptedTokens

      def c: Char = if(index >= line.length) line.last else line(index)
      while(index < line.length && c.isWhitespace) {
        index += 1
      }
      while(index < line.length && !comment) {
        //println(c + " - Index: " + index + "/" + line.length)
        if(!c.isWhitespace) PotentialTokens = PotentialTokens.filter(t => (tokenIndex < t.length) && (t(tokenIndex) == c))
        if((!ValidRegisterLabel(token + c, false) && !ValidConstant(token + c) && PotentialTokens.isEmpty) || (c.isWhitespace && !token.isEmpty)) {
          val tokenValue = TokenMap(token)
          if(tokenValue != -1) {
            //println("Valid")
            tokenList = tokenList.::(tokenValue)
            if(flag =="-s" && token != "//") println((TokenNotation(token),"\"" + token + "\""))
            if(token == "//") comment = true
            token = ""
            tokenIndex = 0
            PotentialTokens = AcceptedTokens
          }
          else {
            PotentialTokens = AcceptedTokens
            if(c.isWhitespace && token.isEmpty) {
              index += 1
              token = ""
              tokenIndex = 0
            } else {
              //println("Invalid")
              token += c
              //tokenList = tokenList.::(tokenValue)
              if(flag == "-p" || flag == "-r") System.err.println("SCANNING ERROR [" + lineNumber + "]: " + token + " is not a valid word")
              tokenList = tokenList.::(tokenValue)
              errorState = true
              token = ""
              tokenIndex = 0
              index += 1

            }

          }
        } else {
          if(!c.isWhitespace) {token += c; tokenIndex += 1}
          index += 1
        }
      }
      if((TokenMap(token) != -1)) tokenList = tokenList.::(TokenMap(token))
      tokenList
    }
  }

  class ILOCParser {
    def parse(line: List[Any], lineNumber: Integer): Option[Operation] = {
      if(line.isEmpty) return Option.empty
      val head = line.head
      val tail = line.tail
      head match {
        case 0 | 2 => validMemOp(head.asInstanceOf[Int], tail, lineNumber)
        case 1 => validLoadI(tail, lineNumber) //Option(validLoadI(tail))
        case 3 | 4 | 5 | 6 | 7  => validArithOp(head.asInstanceOf[Int] ,tail,lineNumber) //Option(validArithOp(tail))
        case 8 => validOutput(tail, lineNumber)//Option(validOutput(tail))
        case 9 => validNop(tail,lineNumber) //Option(validNop(tail))
        case _=>
          if(flag == "-p" || flag == "-r") {
            System.err.println("PARSING ERROR [" + lineNumber + "]: Operation begins with invalid OpCode")
            errorCount += 1
          }
          Option.empty
      }
    }

    def validMemOp(opCode: Int, tokens: List[Any], lineNumber: Integer): Option[MemOp] = {
      if(tokens.isEmpty || !tokens.head.isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") { System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Source Register in Memory Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 2 || tokens(1) != 12) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing \'=>\' in Memory Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 3 || !tokens(2).isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Target Register in Memory Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length > 3) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      }
      else {
        opCode match {
          case 0 => Option(Load(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register]))
          case 2 => Option(Store(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register]))
        }
      }
    }


    def validLoadI(tokens: List[Any], lineNumber: Int): Option[LoadI] = {
      if(tokens.isEmpty || !tokens.head.isInstanceOf[Constant]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Constant in LoadI Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 2 || tokens(1) != 12) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing \'=>\' in Memory Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 3 || !tokens(2).isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Target Register in LoadI Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length > 3) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      } else {
        Option(LoadI(tokens.head.asInstanceOf[Constant],tokens(2).asInstanceOf[Register]))
      }
    }

    def validArithOp(opCode: Int, tokens: List[Any], lineNumber: Int): Option[ArithOp] = {
      if(tokens.isEmpty || !tokens.head.isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing First Source Register in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 2 || tokens(1) != 11) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Comma Separation in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 3 || !tokens(2).isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Second Source Register in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 4 || tokens(3) != 12) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing \'=>\' in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length < 5 || !tokens(4).isInstanceOf[Register]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Target Register in Arithmetic Operation"); errorCount += 1}
        Option.empty
      } else if(tokens.length > 5) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      } else {
        opCode match {
          case 3 => Option(Add(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
          case 4 => Option(Sub(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
          case 5 => Option(Mult(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
          case 6 => Option(LShift(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
          case 7 => Option(RShift(tokens.head.asInstanceOf[Register],tokens(2).asInstanceOf[Register],tokens(4).asInstanceOf[Register]))
        }
      }
    }

    def validOutput(tokens: List[Any], lineNumber: Int): Option[Output] = {
      if(tokens.isEmpty || !tokens.head.isInstanceOf[Constant]) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Missing Constant for Output"); errorCount += 1}
        Option.empty
      } else if(tokens.length > 1) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      }else {
        Option(Output(tokens.head.asInstanceOf[Constant]))
      }
    }

    def validNop(tokens: List[Any], lineNumber: Int): Option[Nop] = {
      if(tokens.nonEmpty) {
        if(flag == "-p" || flag == "-r") {System.err.println("PARSING ERROR [" + lineNumber + "]: Extraneous token at end of Line"); errorCount += 1}
        Option.empty
      } else {
        Option(Nop())
      }
    }
  }

  case class Load(r1: Register, r2: Register) extends MemOp(r1: Register, r2: Register) {
    override def execute(): Unit = ???
  }
  case class Store(r1: Register, r2:Register) extends MemOp(r1: Register, r2: Register) {
    override def execute(): Unit = ???
  }

  abstract class MemOp(r1: Register, r2: Register) extends Operation
  case class LoadI(x: Constant, r: Register) extends Operation {
    override def execute(): Unit = ???
  }



  case class Add(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???
  }

  case class Sub(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???
  }

  case class Mult(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???
  }

  case class LShift(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???
  }

  case class RShift(r1: Register, r2: Register, r3: Register) extends ArithOp(r1: Register, r2: Register, r3: Register) {
    override def execute(): Unit = ???
  }

  abstract class ArithOp(r1: Register, r2: Register, r3: Register) extends Operation

  case class Output(c: Constant) extends Operation {
    override def execute(): Unit = ???
  }

  case class Nop() extends Operation {
    override def execute(): Unit = ???
  }

  abstract class Operation {
    def execute(): Unit
  }

  case class Register(label: Int, value: Option[Integer]) {

  }

  case class Constant(value: Int) {

  }

}

