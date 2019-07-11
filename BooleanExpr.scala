package expr.boolean

import sexpr._

/**
 * This object defines types for creating and evaluating boolean
 * expressions. In particular, it should define the six different
 * boolean expressions that are based on those defined by the
 * boolean sexpr language - except, here they are actual case
 * classes rather than s-expression data types.
 *
 * You must define six case classes that extend Expr (see below):
 *
 * (1) True
 * (2) False
 * (3) Not(e)
 * (4) And(e1,e2)
 * (5) Or(e1,e2)
 * (6) If(c,e1,e2)
 *
 * Note, that case classes are automatically provided a toString
 * method that will resemble how they were constructed. For example,
 *
 * scala> True
 * "True"
 * scala> Not(True)
 * "Not(True)"
 *
 * We will use these strings to test your code - so you must ensure
 * that your case classes exactly resemble the 1-6 definitions above.
 *
 * You must also implement the `parse` and `eval` methods defined
 * below. These methods will parse an SExpr type into a Expr type and
 * evaluate an Expr to an Expr respectively.
 */
object BooleanExpr extends BooleanLanguage {
  sealed trait Expr

  // TODO: Define your case classes here.
    case object True extends Expr
    case object False extends Expr
    case class Not(e: Expr) extends Expr
    case class And(e1: Expr,e2: Expr) extends Expr
    case class Or(e1: Expr,e2: Expr) extends Expr
    case class If(c:Expr,e1: Expr,e2: Expr) extends Expr


  // TODO: implement the parse method.
  // The parse method translates an SExpr type into the Expr types
  // that you defined above. This allows us to define boolean
  // expressions using sexpr form - which decouples your
  // implementation from definitions of boolean expression.
  //
  // Example:
  //
  // scala> parse(&&(T,F))
  // And(True,False)
  //
  // You will need to pay attention to the structure of the sexprs
  // for proper translation.
  //
  // You must you pattern matching and recursion in your
  // implementation.
  //
  // If you are given an improperly formatted boolean sexpr you must
  // throw an IllegalArgumentException.
  def parse(ss: SExpr): Expr = ss match {
    case T=> True
    case F=>False

    case SCons(AND,SCons(x,SCons(y,z)))=> z match {
      case SNil=> And(parse(x), parse(y))       // if there are only two elements after symbol, parse both and print
      case _=>And(parse(x),parse(SCons(y,z)))          // if there are more than two, parse the first, and the rest recursively
    }
    case SCons(NOT,SCons(x,y))=> y match{
      case SNil=>Not(parse(x))              //only one element after the symbol,parse it
      case _=>Not(parse(SCons(x,y)))          // more to parse.. recursively
    }
    case SCons(OR,SCons(x,SCons(y,z)))=> z match {
      case SNil=> Or(parse(x),parse(y))
      case _=>Or(parse(x),parse(SCons(y,z)))    // more to parse.. recursively
    }
    case SCons(IF,SCons(c,SCons(y,SCons(z,w))))=> w match {
      case SNil=> If(parse(c),parse(y),parse(z))
      case _=> If(parse(c),parse(y),parse(SCons(z,w)))    // more to parse.. recursively
    }
    case _ => throw new IllegalArgumentException()
  }

  // TODO: implement the eval method.
  //
  // The eval function "evaluates" a boolean expression. Your function
  // must implement the following rules:
  //
  // (1)  true                  => true
  // (2)  false                 => false
  // (3)  !true                 => false
  // (4)  !false                => true
  // (5)  true && e             => e
  // (6)  false && e            => false
  // (7)  true || e             => true
  // (8)  false || e            => e
  // (9)  if(true) e1 else e2   => e1
  // (10) if(false) e1 else e2  => e2
  //
  // Here are some things you need to consider:
  //
  // (1) When you see an expression e on the right-hand side of a
  //     rule you will need to recursively evaluate e.
  //
  // (2) Although we have given you most rules above, there are other
  //     fules that you will need to implement for proper evaluation.
  //     That is, you must adhere to (4) below for your implementation
  //     to be correct.
  //
  // (3) You need to consider commutativity of operators.
  //
  // (4) Your eval function will always evaluate the given expression
  //     to either the true or false boolean expression.
  //
  def eval(e: Expr): Expr = e match{
    case True=>True

    case False=>False

    case Not(x)=> x match {
      case True=>False
      case False=>True
      case _=> eval(Not(eval(x)))  // eval the rest recursively
    }

    case And(x,y)=> (x,y) match {
      case (True,False)=>False         // list all possibilities
      case (False,True)=>False
      case (True,True)=>True
      case (False,False)=>False
      case _=> eval(And(eval(x),eval(y)))
    }
    case Or(x,y)=>(x,y) match{
      case (True,False)=>True           // list all possibilities
      case (False,True)=>True
      case (True,True)=>True
      case (False,False)=>False
      case _=> eval(Or(eval(x),eval(y)))
    }
    case If(c,a,b)=> (c,a,b) match{
      case (True,True,True)=>True        // list all possibilities
      case (True,False,True)=>False
      case (True,True,False)=>True
      case (True,False,False)=>False

      case (False,True,True)=>True
      case (False,False,True)=>True          // list all possibilities
      case (False,True,False)=>False
      case (False,False,False)=>False
      case _=> eval (If(eval(c),eval(a),eval(b)))
    }
    case _=>False                  // everything else is false
  }
}
