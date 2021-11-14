// Support Code

// *** MAKE NO CHANGES TO THIS FILE ***

//////////////////// PARSER ////////////////////

package base

import scala.util.parsing.combinator.syntactical._

object Parser extends StandardTokenParsers {
  lexical.reserved += ("output", "input",
                       "if", "then", "else",
                       "while", "do",
                       "begin", "end",
                       "var", "proc", "is", "array",
                       "ref")
  lexical.delimiters ++= List("(", ")", "[", "]", ",", ";", ":=",
                              "+", "-", "*", "<", "<=", "=", "||")

  def program: Parser[Stmt] = stmt

  def binops(e: Parser[Expr], ops: (String, (Expr,Expr) => Expr)*) = {
    val sep: Parser[(Expr,Expr) => Expr] = {
      ops.map(op => keyword(op._1) ^^^ op._2)
         .reduce(_ | _)
    }
    chainl1(e,sep)
  }

  def expr0: Parser[Expr] = (
      "input" ~ "(" ~ ")" ^^^ Input
    | numericLit ^^ (num => Num(Integer.parseInt(num)))
    | (ident <~ "[") ~ (expr <~ "]") ^^ (p => ArrayLookup(p._1,p._2))
    | ident ^^ (name => Var(name))
    | "(" ~> expr <~ ")"
  )
  def expr1: Parser[Expr] = (
      "-" ~> expr1 ^^ (e => Neg(e))
    | expr0
  )
  def expr2: Parser[Expr] = binops(expr1, "*" -> Mult)
  def expr3: Parser[Expr] = binops(expr2, "+" -> Plus, "-" -> Minus)
  def expr4: Parser[Expr] = binops(expr3, "=" -> Equals,
                                          "<" -> Less,
                                          "<=" -> LessEq)
  def expr: Parser[Expr] = binops(expr4, "||" -> Or)
  def stmt: Parser[Stmt] = (
      "output" ~ "(" ~ expr ~ ")" ~ ";" ^^ {
        case _ ~ _ ~ e ~ _ ~ _ => Output(e)
      }
    | "if" ~ expr ~ "then" ~ stmt ~ opt("else" ~> stmt) ^^ {
        case _ ~ e ~ _ ~ s1 ~ s2opt => If(e,s1,s2opt)
      }
    | "while" ~ expr ~ "do" ~ stmt ^^ {
        case _ ~ e ~ _ ~ s => While(e,s)
      }
    | "begin" ~ (decl.*) ~ (stmt.*) ~ "end" ^^ {
        case _ ~ decls ~ stmts ~ _ => Block(decls,stmts)
      }
    | ident ~ ":=" ~ expr ~ ";" ^^ {
        case name ~ _ ~ e ~ _ => Assign(name,e)
      }
    | ident ~ "[" ~ expr ~ "]" ~ ":=" ~ expr ~ ";" ^^ {
        case name ~ _ ~ index ~ _ ~ _ ~ e ~ _ => ArrayAssign(name,index,e)
      }
    | ident ~ "(" ~ repsep(expr, ",") ~ ")" ~ ";" ^^ {
        case name ~ _ ~ args ~ _ ~ _ => ProcedureCall(name,args)
      }
  )
  def decl: Parser[Decl] = (
      "var" ~ ident ~ ":=" ~ expr ~ ";" ^^ {
        case _ ~ name ~ _ ~ x ~ _ => VariableDecl(name,x)
      }
    | "proc" ~ ident ~ "(" ~ repsep(param,",") ~ ")" ~ "is" ~ stmt ^^ {
        case _ ~ name ~ _ ~ params ~ _ ~ _ ~ body => ProcedureDecl(name,params,body)
      }
    | "array" ~ ident ~ "[" ~ expr ~ "]" ~ ";" ^^ {
        case _ ~ name ~ _ ~ size ~ _ ~ _ => ArrayDecl(name,size)
      }
  )
  def param: Parser[Param] = (
      "ref" ~> ident ^^ (name => Param(name,true))
    | ident ^^ (name => Param(name,false))
  )

  def parse(text: String) = phrase(program)(new lexical.Scanner(text))
}
