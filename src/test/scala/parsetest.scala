import org.scalatest.FunSuite

import scala.util.parsing.combinator._
import scala.collection.{mutable, immutable}

class VarHolder
{
    var varValues = new immutable.HashMap[String, Double]()
    
    def setVar( id : String, value : Double )
    {
        varValues += id -> value
    }
    def getVar( id : String ) = varValues.get(id)
}

class ExecutionContext
{
    var varStack = List( new VarHolder() )
    
    def push()
    {
        varStack = new VarHolder() :: varStack
    }
    
    def pop()
    {
        varStack = varStack.tail
    }
    
    def setVar( id : String, value : Double ) = { varStack.head.setVar( id, value ) }
    def getVar( id : String ) =
    {
        def getRec( id : String, stack : List[VarHolder] ) : Double = stack.head.getVar(id) match
        {
            case Some(value) => value
            case _ => getRec( id, stack.tail )
        }
        
        getRec( id, varStack )
    }
}

sealed abstract class ExprType

case class Untyped extends ExprType
case class ExprTypeDouble extends ExprType
case class ExprTypeBoolean extends ExprType
case class ExprTypeInteger extends ExprType


sealed abstract class ValueType[T]

case class UnitValue extends ValueType[Untyped]

case class DoubleValue( val v: Double ) extends ValueType[ExprTypeDouble]
case class BooleanValue( val v : Boolean ) extends ValueType[ExprTypeBoolean]
case class IntegerValue( val v : Integer ) extends ValueType[ExprTypeInteger]


sealed abstract class Expression
{
    def exprType = new Untyped()
}

case class NullExpression extends Expression
case class Constant( value : Double ) extends Expression
case class Addition( left : Expression, right : Expression ) extends Expression
case class Subtraction( left : Expression, right : Expression ) extends Expression
case class Multiplication( left : Expression, right : Expression ) extends Expression
case class Division( left : Expression, right : Expression ) extends Expression
case class VarDefinition( id : String, value : Expression ) extends Expression
case class Identifier( name : String ) extends Expression
case class ExprList( val elements : List[Expression] ) extends Expression
case class BlockScopeExpression( val contents : Expression ) extends Expression
case class IfExpression( val cond : Expression, val trueBranch : Expression, val falseBranch : Expression ) extends Expression


class DynamicEvaluator
{
    val context = new ExecutionContext()
    
    def eval( expr : Expression ) : Double =
    {
        expr match
        {
            case NullExpression()                   => 0.0
            case Constant( value )                  => value
            case Addition( left, right )            => eval(left) + eval(right)
            case Subtraction( left, right )         => eval(left) - eval(right)
            case Multiplication( left, right )      => eval(left) * eval(right)
            case Division( left, right )            => eval(left) / eval(right)
            case VarDefinition( name, value )       =>
            {
                val res = eval(value)
                context.setVar( name, res )
                res
            }
            case Identifier( name )                 => context.getVar(name)
            case ExprList( elements )               => elements.foldLeft(0.0)( (x, y) => eval(y) )
            case BlockScopeExpression( contents )   =>
            {
                context.push()
                val res = eval(contents)
                context.pop()
                
                res
            }
            case IfExpression( cond, trueBranch, falseBranch )  =>
            {
                /*if ( eval( cond ) ) eval(trueBranch)
                else eval( falseBranch )*/
                0.0
            }
        }
    }
}


class CalcParseError( msg : String ) extends RuntimeException(msg)

object CalculatorDSL extends JavaTokenParsers
{
    def expr: Parser[Expression] = term ~ ((("+"|"-") ~ expr)?) ^^ {
        case e ~ None => e
        case l ~ Some("+" ~ r)    => new Addition( l, r )
        case l ~ Some("-" ~ r)    => new Subtraction( l, r )
    }
    def term: Parser[Expression] = factor ~ ((("*"|"/") ~ term)?) ^^ {
        case e ~ None => e
        case l ~ Some("*" ~ r)   => new Multiplication( l, r )
        case l ~ Some("/" ~ r)    => new Division( l, r )
    }
    def factor: Parser[Expression] = varDefn | blockScope | controlFlow | identExpr | fpLit | "(" ~> expr <~ ")" ^^ { e => e }
    def fpLit : Parser[Expression] = floatingPointNumber ^^ { fpLit => new Constant( fpLit.toDouble ) }
    
    def identExpr : Parser[Expression] = ident ^^ { x => new Identifier( x ) }
    
    def varDefn : Parser[Expression] = "let" ~ ident ~ "=" ~ expr ^^ {
        case "let" ~ id ~ "=" ~ e => new VarDefinition( id, e )
    }
    
    def exprList : Parser[ExprList] = expr ~ ((";" ~ exprList)?) ^^ {
        case e ~ None           => new ExprList( e :: Nil )
        case e ~ Some(";" ~ eL) => new ExprList( e :: eL.elements )
    }
    
    def controlFlow : Parser[Expression] =
        "if" ~ "(" ~ expr ~ ")" ~ blockScope ~ (("else" ~ blockScope)?) ^^
        {
            case "if" ~ "(" ~ cond ~ ")" ~ trueBranch ~ None                        => new IfExpression( cond, trueBranch, new NullExpression() )
            case "if" ~ "(" ~ cond ~ ")" ~ trueBranch ~ Some("else" ~ falseBranch)  => new IfExpression( cond, trueBranch, falseBranch )
        }
    
    def blockScope : Parser[Expression] = "{" ~> exprList <~ "}" ^^ { e => new BlockScopeExpression( e ) }
    
    
    def parse( expression : String ) =
    {
        parseAll( exprList, expression ) match
        {
            case NoSuccess( msg, next ) => throw new CalcParseError( msg )
            case Success( ast, next ) => ast
        }
    }
}

class CalculatorParseTest extends FunSuite
{
    def exec( str : String ) =
    {
        val parsed = CalculatorDSL.parse( str )
        val evaluator = new DynamicEvaluator()
        
        evaluator.eval( parsed )
    }
    
    test("Simple parse test")
    {
        assert( exec( "(4.0+5.0)/3.0" ) === 3.0 )
        assert( exec( "1.0+2.0+3.0" ) === 6.0 )
        assert( exec( "1.0*2.0*3.0" ) === 6.0 )
        assert( exec("2.0+3.0*3.0") === 11.0 )
        assert( exec("2.0*3.0+3.0") === 9.0 )
        assert( exec( "let x = 12.0" ) === 12.0 )
        assert( exec( "let x = 12.0; x * x" ) === 144.0 )
        assert( exec( "3.0 ; 4.0 ; 5.0" ) === 5.0 )
        
        assert( exec(
            "let y = 10;" +
            "let z = 13;" +
            "y * z"
        ) === 130 )
        
        assert( exec( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 } } }" ) === 3.0 )
        assert( exec( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 }; 6.0 }; 7.0 }" ) === 7.0 )
        assert( exec( "let y = 10.0; { let y = 13.0; y }" ) === 13.0 )
    }
}
