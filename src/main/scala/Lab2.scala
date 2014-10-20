object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
   */

  /*
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /* We represent a variable environment is as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if(b) 1 else 0
      case S(s) => s.toDouble
      //case Function(_,_,_) => Double.NaN
      //case _ => throw new UnsupportedOperationException
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if(n==0) false else true
      case S(s) => if(s.toDouble ==0) false else true
      //case Function(_,_,_) => true
      //case _ => throw new UnsupportedOperationException
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case B(b) => if(b) "true" else "false"
      case N(n) => if(n.isNaN) "Not a Number" else n.toString()
      //case Function(_,_,_) => "function"
      //case _ => throw new UnsupportedOperationException
    }
  }
  
  def eval(env: Env, e: Expr): Expr = {
  	def eToVal(e: Expr): Expr = eval(env, e)
    e match {
      /* Base Cases */
      case _  if(isValue(e)) => e
      case Var(x) => { var x1 = get(env, x); return x1}
      case ConstDecl(x, e1, e2) => {var env2: Env = extend(env, x, eval(env, e1)); eval(env2, e2)}
      case Unary(uop, e1) => uop match {
      	case Neg => return N(-1*toNumber(eval(env, e1)))
      	case Not => return B(!toBoolean(eval(env, e1)))
	  }
      case Binary(bop, e1, e2) => bop match {
      case Plus => {
      	(e1, e2) match 
      	{
      		case (S(s), N(n)) => return S(toStr(eval(e1)) + toStr(eval(e2)))
      		case (N(n), S(s)) => return S(toStr(eval(e1)) + toStr(eval(e2)))
      		case (Var(x), N(n)) => return N(toNumber(eval(e1)) + toNumber(eval(e2)))
      		case _ => return N(toNumber(eval(e1)) + toNumber(eval(e2)))
      	}
      }
      case Minus => return N(toNumber(eval(e1)) - toNumber(eval(e2)))
      case Times => return N(toNumber(eval(e1)) * toNumber(eval(e2)))
      case Div =>
        val ve2 = toNumber(eval(e2))
        val ve1 = toNumber(eval(e1))
        if (ve2 != 0) 
        {
          return N(ve1 / ve2) 
        }
        else 
        {
          if (ve1 >= 0) 
          {
            return N(Double.PositiveInfinity) 
          }
          else 
          {
            return N(Double.NegativeInfinity)
          }
        }

      case Eq => {
      	(eval(env, e1), eval(env, e2)) match {
      	case (Undefined, Undefined) => return B(true)
      	case (Undefined, e2) => return B(false)
      	case (e1, Undefined) => return B(false)
      	case _ => return B(toNumber(eval(env, e1)) == toNumber(eval(env, e2)))
        }
      }
      case Ne => return B(toNumber(eval(env, e1)) != toNumber(eval(env, e2)))
      case Lt => return B(toNumber(eval(env, e1)) < toNumber(eval(env, e2)))
      case Le => return B(toNumber(eval(env, e1)) <= toNumber(eval(env, e2)))
      case Gt => return B(toNumber(eval(env, e1)) > toNumber(eval(env, e2)))
      case Ge => return B(toNumber(eval(env, e1)) >= toNumber(eval(env, e2)))
      case And => {if(toBoolean(eval(env, e1))) eval(env, e2) else eval(env, e1)}
      case Or => {if(toBoolean(eval(env, e1))) eval(env, e1) else eval(env, e2)}
      case Seq => return {eval(env, e1); eval(env, e2)}

    }
    case If(e1, e2, e3) => if(toBoolean(eval(env, e1))) N(toNumber(eval(env, e2)))
    	else N(toNumber(eval(env, e3)))
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eToVal(e1))); Undefined


    }
  }
    
  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(Parser.parse(s))

 /* Interface to run your interpreter from the command-line.  You can ignore what's below. */ 
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(pretty(v))
  }
 
}