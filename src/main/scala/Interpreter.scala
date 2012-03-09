package org.seacourt.pacatoon


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

case class BuiltInFunction( numArgs : Int, fn : List[BaseValue] => BaseValue ) extends BaseValue
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





class TypeError( msg : String ) extends RuntimeException(msg)
class VariableNotFoundError( msg : String ) extends RuntimeException(msg)
class AssertionFailure( msg : String ) extends RuntimeException(msg)

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
        setVar( "import", new BuiltInFunction( 1, args =>
        {
            if ( args.length != 1 ) throw new TypeError( "import function takes only one parameter" )
            
            args(0) match
            {
                case StringValue(fname) =>
                {
                    val f = scala.io.Source.fromFile( fname )
                    val str = f.mkString
                    f.close()
                    val parsed = CalculatorDSL.parse( str )
                    
                    val evaluator = new DynamicASTEvaluator(this)
                    evaluator.eval( parsed )
                }
                case _ => throw new TypeError( "import function requires a string parameter" )
            }
            new UnitValue();
        } ) )
        
        setVar( "assertEqual", new BuiltInFunction( 2, args =>
        {
            if ( args.length != 2 ) throw new TypeError( "assertEqual function takes two parameters" )
            val (a, b) = (args(0), args(1))
            if ( a != b )
            {
                throw new AssertionFailure( a.toString + " != " + b.toString )
            }
            else
            {
                println( "[Assertion passed]" )
            }
            new UnitValue();
        } ) )
        
        setVar( "print", new BuiltInFunction( 1, args =>
        {
            if ( args.length != 1 ) throw new TypeError( "print function takes only one parameter" )
            println( args(0) );
            new UnitValue();
        } ) )
        
        setVar( "toString", new BuiltInFunction( 1, args =>
        {
            if ( args.length != 1 ) throw new TypeError( "toString function takes only one parameter" )
            new StringValue( args(0).toString )
        } ) )
        
        setVar( "nil", new ListTerminatorValue() )
        
        setVar( "head", new BuiltInFunction( 1, args =>
        {
            if ( args.length != 1 ) throw new TypeError( "head function takes only one parameter" )
            
            args(0) match
            {
                case ListElementValue( head, tail ) => head
                case ListTerminatorValue() => throw new TypeError( "Calling head on empty list" )
                case _ => throw new TypeError( "Calling head on non-list type" )
            }
        } ) )
        
        setVar( "tail", new BuiltInFunction( 1, args =>
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


class DynamicASTEvaluator( val context : ExecutionContext )
{
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
                        case BuiltInFunction( numArgs, fn ) =>
                        {
                            if ( numArgs == argList.length )
                            {
                                fn( argList )
                            }
                            else
                            {
                                rawValue
                            }
                        }
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

