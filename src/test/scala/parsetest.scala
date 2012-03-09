import org.scalatest.FunSuite

import scala.util.parsing.combinator._
import scala.collection.{mutable, immutable}


sealed abstract class ExprType

case class Untyped extends ExprType
case class ExprTypeDouble extends ExprType
case class ExprTypeBoolean extends ExprType
case class ExprTypeInteger extends ExprType




sealed abstract class BaseValue

case class UnitValue extends BaseValue
case class DoubleValue( val value: Double ) extends BaseValue
case class BooleanValue( val value : Boolean ) extends BaseValue
case class IntegerValue( val value : Integer ) extends BaseValue
case class FunctionValue( params : List[String], body : Expression ) extends BaseValue


sealed abstract class Expression
{
    def exprType = new Untyped()
}

case class NullExpression extends Expression
case class Constant( value : BaseValue ) extends Expression
case class CmpLt( left : Expression, right : Expression ) extends Expression
case class CmpLe( left : Expression, right : Expression ) extends Expression
case class CmpGt( left : Expression, right : Expression ) extends Expression
case class CmpGe( left : Expression, right : Expression ) extends Expression
case class CmpEq( left : Expression, right : Expression ) extends Expression
case class CmpNe( left : Expression, right : Expression ) extends Expression

case class Addition( left : Expression, right : Expression ) extends Expression
case class Subtraction( left : Expression, right : Expression ) extends Expression
case class Multiplication( left : Expression, right : Expression ) extends Expression
case class Division( left : Expression, right : Expression ) extends Expression

case class IdDefinition( id : String, args : List[String], value : Expression ) extends Expression
case class Apply( lhs : Expression, rhs : Expression ) extends Expression
case class IdExpression( id : String ) extends Expression
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
    
    def lt( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new BooleanValue(v1.value < v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value < v2.value)
            case _  => throw new TypeError( "Arguments to < are not of equal type" )
        }
    }
    
    def le( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new BooleanValue(v1.value <= v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value <= v2.value)
            case _  => throw new TypeError( "Arguments to <= are not of equal type" )
        }
    }
    
    def gt( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new BooleanValue(v1.value > v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value > v2.value)
            case _  => throw new TypeError( "Arguments to > are not of equal type" )
        }
    }
    
    def ge( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new BooleanValue(v1.value >= v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value >= v2.value)
            case _  => throw new TypeError( "Arguments to >= are not of equal type" )
        }
    }
    
    def eq( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new BooleanValue(v1.value == v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value == v2.value)
            case _  => throw new TypeError( "Arguments to == are not of equal type" )
        }
    }
    
    
    def ne( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : DoubleValue, v2 : DoubleValue) => new BooleanValue(v1.value != v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value != v2.value)
            case _  => throw new TypeError( "Arguments to != are not of equal type" )
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
            
            case CmpLt( left, right )               => evaluator.lt( eval(left), eval(right) )
            case CmpLe( left, right )               => evaluator.le( eval(left), eval(right) )
            case CmpGt( left, right )               => evaluator.gt( eval(left), eval(right) )
            case CmpGe( left, right )               => evaluator.ge( eval(left), eval(right) )
            case CmpEq( left, right )               => evaluator.eq( eval(left), eval(right) )
            case CmpNe( left, right )               => evaluator.ne( eval(left), eval(right) )
            
            case Addition( left, right )            => evaluator.add( eval(left), eval(right) )
            case Subtraction( left, right )         => evaluator.subtract( eval(left), eval(right) )
            case Multiplication( left, right )      => evaluator.multiply( eval(left), eval(right) )
            case Division( left, right )            => evaluator.divide( eval(left), eval(right) )
            
            case IdDefinition( name, args, value )  =>
            {
                if ( args == Nil )
                {
                    context.setVar( name, eval(value) )
                }
                else
                {
                    context.setVar( name, new FunctionValue( args, value ) )
                }
                new UnitValue()
            }
            case IdExpression( name )           => context.getVar(name)
            case Apply( lhs, rhs )              =>
            {
                /*val applyVal = context.getVar(name)
                
                applyVal match
                {
                    case FunctionValue( args, body ) =>
                    {
                        if ( param == None ) applyVal
                        else
                        {
                            new UnitValue()
                        }
                    }
                    case _ =>
                    {
                        assert( param == None )
                        applyVal
                    }
                }*/
                new UnitValue()
            }
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
                eval(cond) match
                {
                    case BooleanValue(true)     => eval(trueBranch)
                    case BooleanValue(false)    => eval(falseBranch)
                    case _                      => throw new TypeError( "If expression condition is not of boolean type" )
                }
            }
        }
    }
}




object CalculatorDSL extends JavaTokenParsers
{
    def buildApply( initial : Expression, terms : List[Expression] ) = new NullExpression()
    
    def expr: Parser[Expression] = term1 ~ ((("<="|">="|"=="|"!="|"<"|">") ~ expr)?) ^^
    {
        case e ~ None => e
        case l ~ Some("<=" ~ r)     => new CmpLe( l, r )
        case l ~ Some(">=" ~ r)     => new CmpGe( l, r )
        case l ~ Some("==" ~ r)     => new CmpEq( l, r )
        case l ~ Some("!=" ~ r)     => new CmpNe( l, r )
        case l ~ Some("<" ~ r)      => new CmpLt( l, r )
        case l ~ Some(">" ~ r)      => new CmpGt( l, r )
    }
    
    def term1: Parser[Expression] = term0 ~ ((("+"|"-") ~ term1)?) ^^ {
        case e ~ None => e
        case l ~ Some("+" ~ r)    => new Addition( l, r )
        case l ~ Some("-" ~ r)    => new Subtraction( l, r )
    }
    def term0: Parser[Expression] = factor ~ ((("*"|"/") ~ term0)?) ^^ {
        case e ~ None => e
        case l ~ Some("*" ~ r)   => new Multiplication( l, r )
        case l ~ Some("/" ~ r)    => new Division( l, r )
    }
    def idExpression : Parser[Expression] = ident ^^ { case x => new IdExpression(x) }
    def factor: Parser[Expression] = defn | blockScope | controlFlow | fpLit | "(" ~> expr <~ ")" ^^ { e => e } | applyExpr ^^ { e => e } | idExpression ^^ { e => e }
    def fpLit : Parser[Expression] = floatingPointNumber ^^ { fpLit => new Constant( new DoubleValue(fpLit.toDouble) ) }
    
    //def applyExpr : Parser[Expression] = ident ~ ((expr)?) ^^ { case x ~ param => new Apply( x, param ) }
    def applyExpr : Parser[Expression] = expr ~ ((expr)+) ^^ { case x ~ y => buildApply(x, y)/*new Apply( x, y )*/ }
    
    def defn : Parser[Expression] = "def" ~ ident ~ ((ident)*) ~ "=" ~ expr ^^ {
        case "def" ~ id ~ args ~ "=" ~ e => new IdDefinition( id, args, e )
    }
    
    def exprList : Parser[ExprList] = expr ~ ((";" ~ exprList)?) ^^ {
        case e ~ None           => new ExprList( e :: Nil )
        case e ~ Some(";" ~ eL) => new ExprList( e :: eL.elements )
    }
    
    def controlFlow : Parser[Expression] =
        "if" ~ "(" ~ expr ~ ")" ~ expr ~ (("else" ~ expr)?) ^^
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
    def exec[T]( str : String, dump : Boolean = false ) =
    {
        val parsed = CalculatorDSL.parse( str )
        if (dump) println( parsed )
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
        assert( exec[DoubleValue]( "def x = 12.0; x" ).value === 12.0 )
        assert( exec[DoubleValue]( "def x = 12.0; x * x" ).value === 144.0 )
        assert( exec[DoubleValue]( "3.0 ; 4.0 ; 5.0" ).value === 5.0 )
        
        assert( exec[DoubleValue](
            "def y = 10;" +
            "def z = 13;" +
            "y * z"
        ).value === 130 )
        
        assert( exec[DoubleValue]( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 } } }" ).value === 3.0 )
        assert( exec[DoubleValue]( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 }; 6.0 }; 7.0 }" ).value === 7.0 )
        assert( exec[DoubleValue]( "def y = 10.0; { def y = 13.0; y }" ).value === 13.0 )
        assert( exec[DoubleValue]( "def y = 10.0; def z = y; z" ).value === 10.0 )
        
        assert( exec[BooleanValue]( "4.0 < 5.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 <= 5.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 > 5.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 >= 5.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 == 5.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 != 5.0" ).value === true )
        
        assert( exec[BooleanValue]( "4.0 < 4.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 <= 4.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 > 4.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 >= 4.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 == 4.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 != 4.0" ).value === false )
    }
    
    test("If expression")
    {
        assert( exec[DoubleValue](
            "def x = 12.0;" +
            "def y = 13.0;" +
            "def ret = 0.0;" +
            "if ( x < y ) { 4.0 } else { 5.0 }"
        ).value === 4.0 )    
    }
    
    test("Simple function calls")
    {
        /*assert( exec[DoubleValue]( 
            "def double x = x * 2.0;" +
            "double 5.0", dump=true
        ).value === 10.0 )*/
        
        assert( exec[DoubleValue]( 
            "def sum x y = x + y;" +
            "sum 3.0 5.0", dump=true
        ).value === 8.0 )
    }
    
    /*test("Function calls with arithmetic expressions as arguments")
    {
        assert( exec[DoubleValue]( 
            "def sum x y = x + y;" +
            "sum 2.0+1.0 2.0+3.0", dump=true
        ).value === 8.0 )
    }
     
    test("Partial application syntax(sort of)")
    {
        assert( exec[DoubleValue]( 
            "def sum x y = x + y;" +
            "(sum 3.0) 5.0", dump=true
        ).value === 8.0 )
    }*/
     
    /*test("Simple function calls with variables as args")
    {
        assert( exec[DoubleValue]( 
            "def sum x y = x + y;" +
            "def p = 3.0;" +
            "def q = 5.0;" +
            "sum p q", dump=true
        ).value === 8.0 )
    }

    test("Simple recursion")
    {   
        assert( exec[DoubleValue]( 
            "def sumSeries x = if (x==0) 0.0 else x+(sumSeries (x + (-1)));" +
            "sumSeries 5.0"
        ).value === 15.0 )
    }
    
    test("Function as a first class object" )
    {
        assert( exec[DoubleValue]( 
            "def sum x y = x + y;" +
            "def sumcp = sum;" +
            "sumcp 3.0 5.0"
        ).value === 8.0 )     
    }
    
    test("Function as function parameter" )
    {
        assert( exec[DoubleValue](
            "def sum x y = x + y;" +
            "def mul x y = x + y;" +
            "def apply fn x y = fn x y;" +
            "(apply sum 2.0 3.0) + (apply mul 4.0 5.0)"
        ).value === 25.0 )
    }
    
    test( "Manual partial application" )
    {
        assert( exec[DoubleValue](
            "def sum x y = x + y;" +
            "def double x = { def anon y = x + y; anon };" +
            "double 4.0" ).value === 8.0 )
    }
    
    test("Partial function application" )
    {
        assert( exec[DoubleValue](
            "def sum x y = x + y;" +
            "def inc = sum 1;" +
            "inc 4" ).value === 5.0 )
    }*/
}
