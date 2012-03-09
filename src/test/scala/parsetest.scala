import org.scalatest.FunSuite

import scala.util.parsing.combinator._
import scala.collection.{mutable, immutable}


sealed abstract class ExprType

case class Untyped extends ExprType
case class ExprTypeFloat extends ExprType
case class ExprTypeBoolean extends ExprType
case class ExprTypeInteger extends ExprType




sealed abstract class BaseValue
{
    def toString : String
}

case class UnitValue extends BaseValue
{
    override def toString = "unit"
}

case class FloatValue( val value: Double ) extends BaseValue
{
    override def toString = value.toString
}

case class BooleanValue( val value : Boolean ) extends BaseValue
{
    override def toString = value.toString
}

case class IntegerValue( val value : Integer ) extends BaseValue
{
    override def toString = value.toString
}

case class StringValue( val value : String ) extends BaseValue
{
    override def toString = value
}

case class BuiltInFunction( fn : List[BaseValue] => BaseValue ) extends BaseValue
{
    override def toString = "builtin"
}

case class FunctionValue( params : List[String], body : Expression ) extends BaseValue
{
    override def toString = "fn"
}

case class ApplicationValue( val lhs : BaseValue, val rhs : BaseValue ) extends BaseValue
{
    override def toString = "apply"
}

case class ListTerminatorValue() extends BaseValue
{
    override def toString = "nil"
}

case class ListElementValue( val el : BaseValue, val next : BaseValue ) extends BaseValue
{
    override def toString =
    {
        def rec( m : BaseValue ) : String =
        {
            m match
            {
                case ListElementValue(el, next) => el.toString + "::" + rec(next)
                case ListTerminatorValue() => "nil"
                case _ => throw new TypeError( "Invalid string value" )
            }
        }
        "(" + rec(this) + ")"
    }
}




sealed abstract class Expression
{
    def exprType = new Untyped()
}

case class NullExpression extends Expression
case class Constant( value : BaseValue ) extends Expression


case class BinOpExpression( left : Expression, right : Expression ) extends Expression

case class LogicalAnd( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class LogicalOr( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpLt( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpLe( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpGt( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpGe( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpEq( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpNe( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class ListAppend( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Addition( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Subtraction( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Multiplication( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Division( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )

case class IdDefinition( id : String, args : List[String], value : Expression ) extends Expression
case class Apply( lhs : Expression, rhs : Expression ) extends Expression
case class IdExpression( id : String ) extends Expression
case class ExprList( val elements : List[Expression] ) extends Expression
case class BlockScopeExpression( val contents : Expression ) extends Expression
case class IfExpression( val cond : Expression, val trueBranch : Expression, val falseBranch : Expression ) extends Expression


class ParserError( msg : String ) extends RuntimeException(msg)
class TypeError( msg : String ) extends RuntimeException(msg)
class VariableNotFoundError( msg : String ) extends RuntimeException(msg)

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
    
    // Add built-ins
    {
        setVar( "print", new BuiltInFunction( args =>
        {
            if ( args.length != 1 ) throw new TypeError( "print function takes only one parameter" )
            println( args(0) );
            new UnitValue();
        } ) )
        
        setVar( "toString", new BuiltInFunction( args =>
        {
            if ( args.length != 1 ) throw new TypeError( "toString function takes only one parameter" )
            new StringValue( args(0).toString )
        } ) )
        
        setVar( "nil", new ListTerminatorValue() )
        
        setVar( "head", new BuiltInFunction( args =>
        {
            if ( args.length != 1 ) throw new TypeError( "head function takes only one parameter" )
            
            args(0) match
            {
                case ListElementValue( head, tail ) => head
                case ListTerminatorValue() => throw new TypeError( "Calling head on empty list" )
                case _ => throw new TypeError( "Calling head on non-list type" )
            }
        } ) )
        
        setVar( "tail", new BuiltInFunction( args =>
        {
            if ( args.length != 1 ) throw new TypeError( "tail function takes only one parameter" )
            
            args(0) match
            {
                case ListElementValue( head, tail ) => tail
                case ListTerminatorValue() => throw new TypeError( "Calling head on empty list" )
                case _ => throw new TypeError( "Calling head on non-list type" )
            }
        } ) )
    }
    
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
        def getRec( id : String, stack : List[VarHolder] ) : Option[BaseValue] = stack.head.getVar(id) match
        {
            case Some(value) => Some(value)
            case _ =>
            {
                if (stack.tail == Nil) None
                else getRec( id, stack.tail )
            }
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
            case (v1 : FloatValue, v2 : FloatValue) => new FloatValue(v1.value + v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value + v2.value)
            case (v1 : StringValue, v2 : StringValue) => new StringValue(v1.value + v2.value)
            case _  => throw new TypeError( "Arguments to addition are not of equal type" )
        }
    }
    
    def subtract( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new FloatValue(v1.value - v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value - v2.value)
            case _  => throw new TypeError( "Arguments to subtraction are not of equal type" )
        }
    }
    
    def multiply( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new FloatValue(v1.value * v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value * v2.value)
            case _  => throw new TypeError( "Arguments to multiplication are not of equal type" )
        }
    }
    
    def divide( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new FloatValue(v1.value / v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value / v2.value)
            case _  => throw new TypeError( "Arguments to division are not of equal type" )
        }
    }
    
    def lt( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value < v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value < v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value < v2.value)
            case _  => throw new TypeError( "Arguments to < are not of equal type" )
        }
    }
    
    def le( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value <= v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value <= v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value <= v2.value)
            case _  => throw new TypeError( "Arguments to <= are not of equal type" )
        }
    }
    
    def gt( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value > v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value > v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value > v2.value)
            case _  => throw new TypeError( "Arguments to > are not of equal type" )
        }
    }
    
    def ge( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value >= v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value >= v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value >= v2.value)
            case _  => throw new TypeError( "Arguments to >= are not of equal type" )
        }
    }
    
    def eq( left : BaseValue, right : BaseValue ) = new BooleanValue(left == right)
    /*{
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value == v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value == v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value == v2.value)
            case _  => throw new TypeError( "Arguments to == are not of equal type" )
        }
    }*/
    
    
    def ne( left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value != v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value != v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value != v2.value)
            case _  => throw new TypeError( "Arguments to != are not of equal type" )
        }
    }
}


class DynamicASTEvaluator
{
    val context = new ExecutionContext()
    val evaluator = new ValueEvaluator()
    
    def simplify( rawValue : BaseValue ) : BaseValue =
    {
        rawValue match
        {
            case ApplicationValue( lhs, rhs ) =>
            {
                def simpRec( input : BaseValue, argList : List[BaseValue] ) : BaseValue =
                {
                    input match
                    {
                        case ApplicationValue( lhs, rhs ) => simpRec( lhs, rhs :: argList )
                        case BuiltInFunction( fn ) => fn( argList )
                        case FunctionValue( params, body ) =>
                        {
                            if ( params.length == argList.length )
                            {
                                context.push()
                                params.zip( argList ).foreach
                                {
                                    case (p, a) =>
                                    {
                                        context.setVar( p, a )
                                    }
                                }
                                val res = eval( body )
                                context.pop()
                                res
                            }
                            else
                            {
                                rawValue
                            }
                        }
                        case _ => throw new TypeError( "Malformed application" )
                    }
                }
                
                simpRec( rawValue, List[BaseValue]() )
            }
            case _ => rawValue
        }
    }
    
    // These should be by reference. Functional == immutable, so should be the same as by value but with
    // better memory usage. Also it's incorrect - is able to look at calling function stack frames for vars atm.
    def bindClosureLocals( paramNames : List[String], expr : Expression ) : Expression =
    {
        context.push()
        // Any variables with the same name as function params must not be bound to outer scopes
        paramNames.foreach( param => context.setVar( param, new UnitValue() ) )
        
        def bindRec( expr : Expression ) : Expression =
        {
            expr match
            {
                case NullExpression()                   => new NullExpression()
                case Constant( value )                  => expr
                
                case LogicalAnd( left, right )          => new LogicalAnd( bindRec( left ), bindRec( right ) )
                case LogicalOr( left, right )           => new LogicalOr( bindRec( left ), bindRec( right ) )
                case CmpLt( left, right )               => new CmpLt( bindRec( left ), bindRec( right ) )
                case CmpLe( left, right )               => new CmpLe( bindRec( left ), bindRec( right ) )
                case CmpGt( left, right )               => new CmpGt( bindRec( left ), bindRec( right ) )
                case CmpGe( left, right )               => new CmpGe( bindRec( left ), bindRec( right ) )
                case CmpEq( left, right )               => new CmpEq( bindRec( left ), bindRec( right ) )
                case CmpNe( left, right )               => new CmpNe( bindRec( left ), bindRec( right ) )
                case ListAppend( left, right )          => new ListAppend( bindRec( left ), bindRec( right ) )
                case Addition( left, right )            => new Addition( bindRec( left ), bindRec( right ) )
                case Subtraction( left, right )         => new Subtraction( bindRec( left ), bindRec( right ) )
                case Multiplication( left, right )      => new Multiplication( bindRec( left ), bindRec( right ) )
                case Division( left, right )            => new Division( bindRec( left ), bindRec( right ) )
                
                case IdDefinition( name, args, value )  => new IdDefinition( name, args, bindRec( value ) )
                case IdExpression( name )               =>
                {
                    val res = context.getVar(name)
                    res match
                    {
                        case None               => expr
                        case Some(UnitValue())  => expr
                        case Some(v)            => new Constant(v)
                    }
                }
                case Apply( lhs, rhs )                  => new Apply( bindRec( lhs ), bindRec( rhs ) )
                case ExprList( elements )               => new ExprList( elements.map( x => bindRec(x) ) )
                case BlockScopeExpression( contents )   => 
                {
                    context.push()
                    val res = bindRec( contents )
                    context.pop()
                    
                    new BlockScopeExpression( res )
                }
                case IfExpression( cond, trueBranch, falseBranch )  => new IfExpression( bindRec(cond), bindRec(trueBranch), bindRec(falseBranch) )
            }
        }
        
        val bound = bindRec( expr )
        
        context.pop()
        
        bound
    }
    
    def eval( expr : Expression ) : BaseValue =
    {
        expr match
        {
            case NullExpression()                   => new UnitValue()
            case Constant( value )                  => value
            
            case LogicalAnd( left, right )          =>
            {
                val lvalue = eval(left)
                lvalue match
                {
                    case BooleanValue(false)    => lvalue
                    case BooleanValue(true)     => eval(right)
                    case _                      => throw new TypeError( "Arguments to logical operators must be of boolean type" )
                }
            }

            case LogicalOr( left, right )          =>
            {
                val lvalue = eval(left)
                lvalue match
                {
                    case BooleanValue(true)     => lvalue
                    case BooleanValue(false)    => eval(right)
                    case _                      => throw new TypeError( "Arguments to logical operators must be of boolean type" )
                }
            }
            
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
            
            case ListAppend( left, right )          => new ListElementValue( eval(left), eval(right) )
            
            case IdDefinition( name, args, value )  =>
            {
                val rhs = args match
                {
                    case Nil => simplify( eval(value) )
                    case _   => new FunctionValue( args, bindClosureLocals( args, value ) )
                }
                context.setVar( name, rhs )
                
                rhs
            }
            case IdExpression( name )           => context.getVar(name).get
            case Apply( lhs, rhs )              =>
            {
                simplify( new ApplicationValue( eval(lhs), eval(rhs) ) )
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




object CalculatorDSL extends RegexParsers with PackratParsers
{
    // TODO: Sort keyword parsing out to avoid having the minging '@' prefix
    def ident: Parser[String] = """[a-zA-Z_]\w*""".r
    def wholeNumber: Parser[String] = """-?\d+""".r
    def decimalNumber: Parser[String] = """(\d+(\.\d*)?|\d*\.\d+)""".r
    def stringLiteral: Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
    def floatingPointNumber: Parser[String] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    
    def buildApply( initial : Expression, terms : List[Expression] ) =
    {
        terms.foldLeft( initial )( (lhs, rhs) => new Apply( lhs, rhs ) )
    }
    
    lazy val expr : Parser[Expression] = term4 ~ ((term4)*) ^^ {
        case x ~ Nil    =>  x
        case x ~ y      => buildApply(x, y) }
        
    lazy val term4: Parser[Expression] = term3 ~ ((("&&"|"||") ~ expr)?) ^^
    {
        case e ~ None => e
        case l ~ Some("&&" ~ r)     => new LogicalAnd( l, r )
        case l ~ Some("||" ~ r)     => new LogicalOr( l, r )
    }
    
    lazy val term3: Parser[Expression] = term2 ~ ((("<="|">="|"=="|"!="|"<"|">") ~ expr)?) ^^
    {
        case e ~ None => e
        case l ~ Some("<=" ~ r)     => new CmpLe( l, r )
        case l ~ Some(">=" ~ r)     => new CmpGe( l, r )
        case l ~ Some("==" ~ r)     => new CmpEq( l, r )
        case l ~ Some("!=" ~ r)     => new CmpNe( l, r )
        case l ~ Some("<" ~ r)      => new CmpLt( l, r )
        case l ~ Some(">" ~ r)      => new CmpGt( l, r )
    }
    
    lazy val term2: Parser[Expression] = term1 ~ ((("+"|"-") ~ term2)?) ^^ {
        case e ~ None => e
        case l ~ Some("+" ~ r)      => new Addition( l, r )
        case l ~ Some("-" ~ r)      => new Subtraction( l, r )
    }
    lazy val term1: Parser[Expression] = term0 ~ ((("*"|"/") ~ term1)?) ^^ {
        case e ~ None => e
        case l ~ Some("*" ~ r)      => new Multiplication( l, r )
        case l ~ Some("/" ~ r)      => new Division( l, r )
    }
    
    lazy val term0: Parser[Expression] = factor ~ (("::" ~ term1)?) ^^ {
        case e ~ None => e
        case l ~ Some("::" ~ r)     => new ListAppend( l, r )
    }
    
    lazy val idExpression : Parser[Expression] = ident ^^ { x => new IdExpression(x) }
    
    lazy val factor: Parser[Expression] = defn | blockScope | controlFlow | fpLit | stringLit | "(" ~> expr <~ ")" ^^ { e => e } | idExpression ^^ { e => e }
    
    lazy val fpLit : Parser[Expression] = floatingPointNumber ^^ 
    {
        lit =>
        {
            val isInteger = !lit.foldLeft(false)((x,y) => x || (y=='.'))
            if ( isInteger )
            {
                new Constant( new IntegerValue(lit.toInt) )
            }
            else
            {
                new Constant( new FloatValue(lit.toDouble) )
            }
        } 
    }
    lazy val stringLit : Parser[Expression] = stringLiteral ^^ { str => new Constant( new StringValue( str.drop(1).dropRight(1) ) ) }
 
    lazy val defn : Parser[Expression] = "@def" ~ ident ~ ((ident)*) ~ "=" ~ expr ^^ {
        case "@def" ~ id ~ args ~ "=" ~ e => new IdDefinition( id, args, e )
    }
    
    lazy val exprList : Parser[ExprList] = expr ~ ((";" ~ exprList)?) ^^ {
        case e ~ None           => new ExprList( e :: Nil )
        case e ~ Some(";" ~ eL) => new ExprList( e :: eL.elements )
    }
    
    lazy val controlFlow : Parser[Expression] = "@if" ~ "(" ~ expr ~ ")" ~ expr ~ (("@else" ~ expr)?) ^^
    {
        case "@if" ~ "(" ~ cond ~ ")" ~ trueBranch ~ None                           => new IfExpression( cond, trueBranch, new NullExpression() )
        case "@if" ~ "(" ~ cond ~ ")" ~ trueBranch ~ Some("@else" ~ falseBranch)    => new IfExpression( cond, trueBranch, falseBranch )
    }
    
    lazy val blockScope : Parser[Expression] = "{" ~> exprList <~ "}" ^^ { e => new BlockScopeExpression( e ) }

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
        assert( exec[FloatValue]( "(4.0+5.0)/3.0" ).value === 3.0 )
        assert( exec[FloatValue]( "1.0+2.0+3.0" ).value === 6.0 )
        assert( exec[FloatValue]( "1.0*2.0*3.0" ).value === 6.0 )
        assert( exec[FloatValue]( "2.0+3.0*3.0").value === 11.0 )
        assert( exec[FloatValue]( "2.0*3.0+3.0").value === 9.0 )
        assert( exec[FloatValue]( "@def x = 12.0; x" ).value === 12.0 )
        assert( exec[FloatValue]( "@def x = 12.0; x * x" ).value === 144.0 )
        assert( exec[FloatValue]( "3.0 ; 4.0 ; 5.0" ).value === 5.0 )
        
        assert( exec[FloatValue](
            "@def y = 10.0;" +
            "@def z = 13.0;" +
            "y * z"
        ).value === 130 )
        
        assert( exec[FloatValue]( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 } } }" ).value === 3.0 )
        assert( exec[FloatValue]( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 }; 6.0 }; 7.0 }" ).value === 7.0 )
        assert( exec[FloatValue]( "@def y = 10.0; { @def y = 13.0; y }" ).value === 13.0 )
        assert( exec[FloatValue]( "@def y = 10.0; @def z = y; z" ).value === 10.0 )
        
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
        
        assert( exec[IntegerValue]( "4 * 5" ).value === 20 )
        
        
        //assert( exec[FloatValue]( "@def x : float = 4.0; x" ).value === 4.0 )
        //assert( exec[IntegerValue]( "@def x : int = 4; x" ).value === 4 )
    }
    
    val mapFn = 
        "@def map fn l =" +
        "{" +
        "    @if (l == nil) nil" +
        "    @else (fn (head l)) :: (map fn (tail l))" +
        "};"

    
    test("If expression")
    {
        assert( exec[FloatValue](
            "@def x = 12.0;" +
            "@def y = 13.0;" +
            "@def ret = 0.0;" +
            "@if ( x < y ) { 4.0 } @else { 5.0 }"
        ).value === 4.0 )    
    }
    
    test("If expression with chained elses")
    {
        assert( exec[FloatValue](
            "@if ( 3.0 < 4.0 ) 5.0 @else @if ( 4.0 < 5.0 ) 6.0 @else @if ( 5.0 < 6.0 ) 7.0 @else 8.0"
        ).value === 5.0 )    
    }
    
    test("Simple function calls")
    {
        assert( exec[FloatValue]( 
            "@def double x = x * 2.0;" +
            "double 5.0"
        ).value === 10.0 )
        
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "sum 3.0 5.0"
        ).value === 8.0 )
    }
    
    test( "Built in string conversion" )
    {
        assert( exec[StringValue]( "\"Here \" + (toString 3.0) + \" \" + (toString 5)" ).value === "Here 3.0 5" )
    }
    
    test("Function calls with arithmetic expressions as arguments")
    {
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "sum 2.0+1.0 2.0+3.0"
        ).value === 8.0 )
    }
     
    test("Partial application syntax(sort of)")
    {
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "(sum 3.0) 5.0"
        ).value === 8.0 )
    }
     
    test("Simple function calls with variables as args")
    {
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "@def p = 3.0;" +
            "@def q = 5.0;" +
            "sum p q"
        ).value === 8.0 )
    }

    test("Simple recursion")
    {   
        assert( exec[FloatValue]( 
            "@def sumSeries x = @if (x==0.0) 0.0 @else x+(sumSeries (x + (-1.0)));" +
            "sumSeries 5.0"
        ).value === 15.0 )
    }
    
    test("Function as a first class object" )
    {
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "@def sumcp = sum;" +
            "sumcp 3.0 5.0"
        ).value === 8.0 )     
    }
    
    test("Partial function application" )
    {
        assert( exec[IntegerValue](
            "@def sum x y = x + y;" +
            "@def inc = sum 1;" +
            "inc 4" ).value === 5 )
    }
    
    test("Function as function parameter" )
    {
        assert( exec[FloatValue](
            "@def sum x y = x + y;" +
            "@def mul x y = x * y;" +
            "@def apply fn x y = fn x y;" +
            "(apply sum 2.0 3.0) + (apply mul 4.0 5.0)"
        ).value === 25.0 )
    }
    
    test("Lists 1")
    {
        assert( exec[FloatValue](
            "@def l = 1.0 :: 2.0 :: 3.0 :: nil;" +
            "head l"
        ).value === 1.0 )
    }
    
    test("Lists 2")
    {
        assert( exec[FloatValue](
            "@def l = 1.0 :: 2.0 :: 3.0 :: nil;" +
            "head (tail (tail l))"
        ).value === 3.0 )
    }
    
    test("Lists 3")
    {
        assert( exec[BooleanValue](
            "@def l = 1.0 :: 2.0 :: 3.0 :: nil;" +
            "@def last = tail (tail (tail l));" +
            "last == nil"
        ).value === true )
    }
    
    
    test( "Closures" )
    {
        assert( exec[FloatValue](
            "@def incrementer count = (@def _ x = x+count);" +
            "@def addTwo = incrementer 2.0;" +
            "@def addFive = incrementer 5.0;" +
            "(addTwo 7.0) + (addFive 2.0)"
        ).value === 16.0 )
    }
    
    /*test( "Closures2: Manual (out of order) partial application" )
    {
        assert( exec[FloatValue](
            "@def divide x y = x / y;" +
            "@def fixedDivide x = (@def _ y = divide y / x);" +
            "@def halve = fixedDivide 2.0;" +
            "@def third = fixedDivide 3.0;" +
            "(halve 16.0) + (third 9.0)"
        ).value === 11.0 )
    }*/
    
    test("Blocks")
    {
        assert( exec[FloatValue]( "{ 1.0; 2.0; 3.0; 4.0 }" ).value === 4.0 )
    }
    
    test("Functional tools: map")
    {
        assert( exec[ListElementValue](
            mapFn +
            "map (@def square x=x*x) (1.0::2.0::3.0::4.0::nil)"
        ).toString === "(1.0::4.0::9.0::16.0::nil)" )
    }
    
    test("Functional tools: foldLeft")
    {
        assert( exec[FloatValue](
            "@def foldLeft fn acc l =" +
            "{" +
            "    @if (l == nil) acc" +
            "    @else (fn (head l) (foldLeft fn acc (tail l)))" +
            "};" +
            "foldLeft (@def sum x y=x+y) 0.0 (1.0::2.0::3.0::4.0::nil)"
        ).value === 10.0 )
    }
    
    test( "Mergesort" )
    {
        // Wouldn't it be nice to have pattern matching?
        val mergeFn =
            "@def merge l1 l2 =" +
            "{" +
            "    @if ((l1 == nil) && (l2 == nil)) nil" +
            "    @else @if (l1 == nil) ((head l2) :: (merge l1 (tail l2)))" +
            "    @else @if (l2 == nil) ((head l1) :: (merge (tail l1) l2))" +
            "    @else @if ((head l1) < (head l2)) ((head l1) :: (merge (tail l1) l2))" +
            "    @else ((head l2) :: (merge l1 (tail l2)))" +
            "};"

        assert( exec[ListElementValue]( mergeFn + "merge (1.0 :: 3.0 :: 5.0 :: nil) (0.0 :: 2.0 :: 4.0 :: 6.0 :: nil)" ).toString === "(0.0::1.0::2.0::3.0::4.0::5.0::6.0::nil)" )
        assert( exec[ListElementValue]( mergeFn + "merge (merge (merge (3.0 :: nil) (4.0 :: nil)) (merge (1.0 :: nil) (2.0::nil))) (merge (0.0 :: nil) (5.0::nil))" ).toString === "(0.0::1.0::2.0::3.0::4.0::5.0::nil)" )
        
        assert( exec[ListElementValue](
            mapFn + 
            mergeFn +
            "@def unordered = 5.0::1.0::3.0::2.0::4.0::0.0::6.0::nil;" +
            "@def elsAsLists = map (@def _ x = x::nil) unordered;" +
            "@def mergePairs l =" +
            "{" +
            "    @if (l==nil) nil" +
            "    @else" +
            "    {" +
            "        @def first = head l;" +
            "        @def tail1 = tail l;" +
            "        @if ( tail1 == nil ) first :: nil" +
            "        @else (merge first (head tail1)) :: (mergePairs (tail tail1))" +
            "    }" +
            "};" +
            "@def mergePairsRec l = @if ((tail l) == nil) (head l) @else (mergePairsRec (mergePairs l));" +
            "mergePairsRec elsAsLists" ).toString === "(0.0::1.0::2.0::3.0::4.0::5.0::6.0::nil)" )
            
            
        
        /*val splitFn =
            "@def split l l1 l2=" +
            "{" +
            "    @if ( l == nil ) l1 :: l2 :: nil
            "    @if ( (l != nil) && ((tail l) != nil) ) split (tail (tail l)) (head l) (head (tail l))" +
            "    @else @if ( */
    }
    
    
    /*<<mergesort>>=
        let rec mergesort compare = 
	        split
          in
	        merge
	        in function
          | ([] | [_]) as l -> l
          | l ->  let left,right = split l in 
                  merge compare (mergesort compare left) (mergesort compare right)
        ;;
    */
    
    /*test("Record type")
    {
        exec[FloatValue]( "@type Test = { a : double, b : double }" )
        assert( exec[FloatValue](
            "@type Test = { a : Double, b : Double };" +
            "@def t = Test( a = 12.0, b = 15.0 );" +
            "t.a + t.b"
        ).value === 23.0 )
    }*/
}
