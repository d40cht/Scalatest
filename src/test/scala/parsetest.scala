import org.scalatest.FunSuite

import scala.util.parsing.combinator._
import scala.collection.{mutable, immutable}


sealed abstract class ExprType

case class Untyped extends ExprType
case class ExprTypeDouble extends ExprType
case class ExprTypeBoolean extends ExprType
case class ExprTypeInteger extends ExprType




sealed abstract class BaseValue
abstract class ValueType[T] extends BaseValue

case class UnitValue extends ValueType[Untyped]

case class DoubleValue( val value: Double ) extends ValueType[ExprTypeDouble]
case class BooleanValue( val value : Boolean ) extends ValueType[ExprTypeBoolean]
case class IntegerValue( val value : Integer ) extends ValueType[ExprTypeInteger]


sealed abstract class Expression
{
    def exprType = new Untyped()
}

case class NullExpression extends Expression
case class Constant( value : BaseValue ) extends Expression
case class Addition( left : Expression, right : Expression ) extends Expression
case class Subtraction( left : Expression, right : Expression ) extends Expression
case class Multiplication( left : Expression, right : Expression ) extends Expression
case class Division( left : Expression, right : Expression ) extends Expression
case class VarDefinition( id : String, value : Expression ) extends Expression
case class Identifier( name : String ) extends Expression
case class ExprList( val elements : List[Expression] ) extends Expression
case class BlockScopeExpression( val contents : Expression ) extends Expression
case class IfExpression( val cond : Expression, val trueBranch : Expression, val falseBranch : Expression ) extends Expression

class ParserError( msg : String ) extends RuntimeException(msg)
class TypeError( msg : String ) extends RuntimeException(msg)

class VarHolder
{
    var varValues = new immutable.HashMap[String, BaseValue]()
    
    def setVar( id : String, value : BaseValue )
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
    
    def setVar( id : String, value : BaseValue ) = { varStack.head.setVar( id, value ) }
    def getVar( id : String ) =
    {
        def getRec( id : String, stack : List[VarHolder] ) : BaseValue = stack.head.getVar(id) match
        {
            case Some(value) => value
            case _ => getRec( id, stack.tail )
        }
        
        getRec( id, varStack )
    }
}

class ValueEvaluator
{
    def add( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new DoubleValue(v1.value + v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value + v2.value)
            case _  => throw new TypeError( "Arguments to addition are not of equal type" )
        }
    }
    
    def subtract( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new DoubleValue(v1.value - v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value - v2.value)
            case _  => throw new TypeError( "Arguments to subtraction are not of equal type" )
        }
    }
    
    def multiply( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new DoubleValue(v1.value * v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value * v2.value)
            case _  => throw new TypeError( "Arguments to multiplication are not of equal type" )
        }
    }
    
    def divide( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new DoubleValue(v1.value / v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value / v2.value)
            case _  => throw new TypeError( "Arguments to division are not of equal type" )
        }
    }
}


class DynamicASTEvaluator
{
    val context = new ExecutionContext()
    val evaluator = new ValueEvaluator()
    
    def eval( expr : Expression ) : BaseValue =
    {
        expr match
        {
            case NullExpression()                   => new UnitValue()
            case Constant( value )                  => value
            case Addition( left, right )            => evaluator.add( eval(left), eval(right) )
            case Subtraction( left, right )         => evaluator.subtract( eval(left), eval(right) )
            case Multiplication( left, right )      => evaluator.multiply( eval(left), eval(right) )
            case Division( left, right )            => evaluator.divide( eval(left), eval(right) )
            case VarDefinition( name, value )       =>
            {
                val res = eval(value)
                context.setVar( name, res )
                res
            }
            case Identifier( name )                 => context.getVar(name)
            case ExprList( elements )               => elements.foldLeft(new UnitValue() : BaseValue)( (x, y) => eval(y) )
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
                new UnitValue()
            }
        }
    }
}




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
    def fpLit : Parser[Expression] = floatingPointNumber ^^ { fpLit => new Constant( new DoubleValue(fpLit.toDouble) ) }
    
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
            case NoSuccess( msg, next ) => throw new ParserError( msg )
            case Success( ast, next ) => ast
        }
    }
}

class CalculatorParseTest extends FunSuite
{
    def exec[T]( str : String ) =
    {
        val parsed = CalculatorDSL.parse( str )
        val evaluator = new DynamicASTEvaluator()
        
        evaluator.eval( parsed ).asInstanceOf[T]
    }
    
    test("Simple parse test")
    {
        assert( exec[DoubleValue]( "(4.0+5.0)/3.0" ).value === 3.0 )
        assert( exec[DoubleValue]( "1.0+2.0+3.0" ).value === 6.0 )
        assert( exec[DoubleValue]( "1.0*2.0*3.0" ).value === 6.0 )
        assert( exec[DoubleValue]( "2.0+3.0*3.0").value === 11.0 )
        assert( exec[DoubleValue]( "2.0*3.0+3.0").value === 9.0 )
        assert( exec[DoubleValue]( "let x = 12.0" ).value === 12.0 )
        assert( exec[DoubleValue]( "let x = 12.0; x * x" ).value === 144.0 )
        assert( exec[DoubleValue]( "3.0 ; 4.0 ; 5.0" ).value === 5.0 )
        
        assert( exec[DoubleValue](
            "let y = 10;" +
            "let z = 13;" +
            "y * z"
        ).value === 130 )
        
        assert( exec[DoubleValue]( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 } } }" ).value === 3.0 )
        assert( exec[DoubleValue]( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 }; 6.0 }; 7.0 }" ).value === 7.0 )
        assert( exec[DoubleValue]( "let y = 10.0; { let y = 13.0; y }" ).value === 13.0 )
    }
}
