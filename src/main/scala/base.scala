// Support Code

// *** MAKE NO CHANGES TO THIS FILE ***

package object base {

  //////////////////// ABSTRACT SYNTAX TREES ////////////////////
  // Abstract syntax trees are broken into three main categories:
  //   Expr (for expressions)
  //   Stmt (for statements)
  //   Decl (for declarations)
  // plus a helper class for parameters in a procedure declaration

  ///// EXPRESSIONS /////
  sealed trait Expr
  case object Input extends Expr
  case class Num(value: Int) extends Expr
  case class Neg(expr: Expr) extends Expr // negative
  case class Plus(left: Expr, right: Expr) extends Expr
  case class Minus(left: Expr, right: Expr) extends Expr
  case class Mult(left: Expr, right: Expr) extends Expr
  case class Equals(left: Expr, right: Expr) extends Expr
  case class Less(left: Expr, right: Expr) extends Expr
  case class LessEq(left: Expr, right: Expr) extends Expr // less than or equal
  case class Or(left: Expr, right: Expr) extends Expr // boolean or
  case class Var(name: String) extends Expr
  case class ArrayLookup(name: String, index: Expr) extends Expr

  ///// STATEMENTS /////
  sealed trait Stmt
  case class Output(expr: Expr) extends Stmt
  case class If(condition: Expr, thenStmt: Stmt, elseStmt: Option[Stmt]) extends Stmt
  case class While(condition: Expr, body: Stmt) extends Stmt
  case class Block(decls: List[Decl], stmts: List[Stmt]) extends Stmt
  case class Assign(name: String, expr: Expr) extends Stmt
  case class ArrayAssign(name: String, index: Expr, expr: Expr) extends Stmt
  // ProcedureCall not needed until Part B
  case class ProcedureCall(name: String, args: List[Expr]) extends Stmt

  ///// DECLARATIONS /////
  sealed trait Decl
  case class VariableDecl(name: String, init: Expr) extends Decl
  case class ArrayDecl(name: String, size: Expr) extends Decl
  // ProcedureDecl not needed until Part B
  case class ProcedureDecl(name: String, params: List[Param], body: Stmt) extends Decl

  // This is a helper class used in ProcedureDecl (not needed until Part B)
  case class Param(name: String, byRef: Boolean)

  //////////////////// ENVIRONMENTS AND DENOTABLES ///////////////////
  // A "denotable" is a piece of information that can be given a name.
  // An environment maps each name to the related piece of information.
  // In this language, three kinds of things have names: variables, arrays,
  // and procedures.
  //   - For a variable, the environment holds a Location.
  //   - For an array, the environment holds a MemoryBlock.
  //   - For a procedure, the environment holds a Closure.
  // (Location, MemoryBlock, and Closure are defined later.)

  class Env private (private val map: Map[String, Denotable]) {
    def insert(name: String, value: Denotable): Env =
      new Env(map + (name -> value))
    def lookupVar(name: String): Location = {
      if (!map.contains(name)) error("Unknown variable: " + name)
      map(name) match {
        case loc : Location => loc
        case _ : MemoryBlock => error("Expected variable, found array: " + name)
        case _ : Closure => error("Expected variable, found procedure: " + name)
      }
    }
    def lookupArray(name: String): MemoryBlock = {
      if (!map.contains(name)) error("Unknown array: " + name)
      map(name) match {
        case mem : MemoryBlock => mem
        case _ : Location => error("Expected array, found variable: " + name)
        case _ : Closure => error("Expected array, found procedure: " + name)
      }
    }
    def lookupProcedure(name: String): Closure = {
      if (!map.contains(name)) error("Unknown procedure: " + name)
      map(name) match {
        case closure : Closure => closure
        case _ : Location => error("Expected procedure, found variable: " + name  )
        case _ : MemoryBlock => error("Expected procedure, found array: " + name  )
      }
    }
  }

  // the constructor for the Env class is private, so the companion
  // object provides a way to get a fresh environment
  object Env {
    val empty = new Env(Map.empty)
  }

  // Information that can be named (Location, MemoryBlock, or Closure)
  trait Denotable

  // A Location models an address in memory
  class Location(var contents: Int) extends Denotable

  // A MemoryBlock models a block of addresses in memory
  class MemoryBlock(val locs: Map[Int, Location]) extends Denotable

  // A Closure holds the information the interpreter needs to know about
  // a procedure to be able to call the procedure. (Not needed until Part B.)
  class Closure(val name: String, val params: List[Param], val body: Stmt)
                  extends Denotable {
    var env: Env = null
  }

  //////////////////// INPUT/OUTPUT ////////////////////
  // if interactive, then inputs come from the keyboard, and outputs
  //   go to the screen
  // if not interactive, inputs come from the 'inputs' list and
  //   outputs go to the 'outputs' list
  private var interactive: Boolean = true
  private var inputs: List[Int] = List.empty
  private var outputs: List[Int] = List.empty

  def predefinedInput(): Int = {
    if (interactive) {
      print("Enter a number: ")
      scala.io.StdIn.readLine().trim.toInt
    }
    else {
      val input = inputs.head
      inputs = inputs.tail
      input
    }
  }

  def predefinedOutput(expr: Expr, env: Env): Unit = {
    if (interactive) println(">>> " + hw8.eval(expr,env))
    else outputs = hw8.eval(expr,env) :: outputs
  }


  //////////////////// The main interpreter ////////////////////

  def interpret(program: Stmt) = hw8.exec(program, Env.empty)


  //////////////////// TESTING ////////////////////
  // Use run and test are how to test your interpreter

  def run(text: String): Unit = {
    interactive = true
    Parser.parse(text) match {
      case Parser.Success(program,_) => interpret(program)
      case Parser.Failure(msg,_) => println("Parser error: " + msg)
      case Parser.Error(msg,_) => println("Parser fatal error: " + msg)
    }
  }

  def test(text: String, ins: List[Int], outs: List[Int]): Unit = {
    interactive = false
    inputs = ins
    outputs = Nil
    try {
      Parser.parse(text) match {
        case Parser.Success(program,_) => interpret(program)
        case Parser.Failure(msg,_) => error("Parser error: " + msg)
        case Parser.Error(msg,_) => error("Parser fatal error: " + msg)
      }
      outputs = outputs.reverse
      if (outputs == outs) println("Passed.")
      else {
        println(s"""FAILED ON PROGRAM BELOW
On these inputs  : $ins
Expected outputs : $outs
But got outputs  : $outputs
Program = $text""")
      }
    }
    catch {
      case exn: Exception =>
        println(s"FAILED: ${exn.getMessage}\nProgram = $text")
    }
  }

  //////////////////// ERROR REPORTING ////////////////////

  class BS_Error(msg: String) extends Exception(msg)

  def error(msg: String): Nothing = throw new BS_Error(msg)
}
