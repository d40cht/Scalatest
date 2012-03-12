package org.seacourt.pacatoon


import scala.collection.{mutable, immutable}
import scala.util.parsing.input.{Position, NoPosition}

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

case class BuiltInFunction( numArgs : Int, fn : (Position, List[BaseValue]) => BaseValue ) extends BaseValue
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
                case _ => throw new TypeError( NoPosition, "Invalid string value" )
            }
        }
        "(" + rec(this) + ")"
    }
}



class PositionedException( position : Position, msg : String ) extends RuntimeException( "("+position.line+", "+position.column+"): " + msg )

class TypeError( _position : Position, _msg : String ) extends PositionedException(_position, _msg)
class VariableNotFoundError( _position : Position, _msg : String ) extends PositionedException(_position, _msg)
class AssertionFailure( _position : Position, _msg : String ) extends PositionedException(_position, _msg)



class ValueExecutionContext extends ExecutionContextBase( () => new ContextFrame[BaseValue]() )
{
    // Add built-ins
    {
        set( "import", new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "import function takes only one parameter" )
            
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
                case _ => throw new TypeError( pos, "import function requires a string parameter" )
            }
            new UnitValue();
        } ) )
        
        set( "assertEqual", new BuiltInFunction( 2, (pos, args) =>
        {
            if ( args.length != 2 ) throw new TypeError( pos, "assertEqual function takes two parameters" )
            val (a, b) = (args(0), args(1))
            if ( a != b )
            {
                throw new AssertionFailure( pos, a.toString + " != " + b.toString )
            }
            else
            {
                println( "[Assertion passed]" )
            }
            new UnitValue();
        } ) )
        
        set( "print", new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "print function takes only one parameter" )
            println( args(0) );
            new UnitValue();
        } ) )
        
        set( "toString", new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "toString function takes only one parameter" )
            new StringValue( args(0).toString )
        } ) )
        
        set( "nil", new ListTerminatorValue() )
        
        set( "head", new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "head function takes only one parameter" )
            
            args(0) match
            {
                case ListElementValue( head, tail ) => head
                case ListTerminatorValue() => throw new TypeError( pos, "Calling head on empty list" )
                case _ => throw new TypeError( pos, "Calling head on non-list type" )
            }
        } ) )
        
        set( "tail", new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "tail function takes only one parameter" )
            
            args(0) match
            {
                case ListElementValue( head, tail ) => tail
                case ListTerminatorValue() => throw new TypeError( pos, "Calling head on empty list" )
                case _ => throw new TypeError( pos, "Calling head on non-list type" )
            }
        } ) )
    }
}

class ValueEvaluator
{
    def add( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new FloatValue(v1.value + v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value + v2.value)
            case (v1 : StringValue, v2 : StringValue) => new StringValue(v1.value + v2.value)
            case _  => throw new TypeError( pos, "Arguments to addition are not of equal type" )
        }
    }
    
    def subtract( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new FloatValue(v1.value - v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value - v2.value)
            case _  => throw new TypeError( pos, "Arguments to subtraction are not of equal type" )
        }
    }
    
    def multiply( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new FloatValue(v1.value * v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value * v2.value)
            case _  => throw new TypeError( pos, "Arguments to multiplication are not of equal type" )
        }
    }
    
    def divide( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new FloatValue(v1.value / v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new IntegerValue(v1.value / v2.value)
            case _  => throw new TypeError( pos, "Arguments to division are not of equal type" )
        }
    }
    
    def lt( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value < v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value < v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value < v2.value)
            case _  => throw new TypeError( pos, "Arguments to < are not of equal type" )
        }
    }
    
    def le( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value <= v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value <= v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value <= v2.value)
            case _  => throw new TypeError( pos, "Arguments to <= are not of equal type" )
        }
    }
    
    def gt( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value > v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value > v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value > v2.value)
            case _  => throw new TypeError( pos, "Arguments to > are not of equal type" )
        }
    }
    
    def ge( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value >= v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value >= v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value >= v2.value)
            case _  => throw new TypeError( pos, "Arguments to >= are not of equal type" )
        }
    }
    
    def eq( pos : Position, left : BaseValue, right : BaseValue ) = new BooleanValue(left == right)
    /*{
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value == v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value == v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value == v2.value)
            case _  => throw new TypeError( "Arguments to == are not of equal type" )
        }
    }*/
    
    
    def ne( pos : Position, left : BaseValue, right : BaseValue ) =
    {
        (left, right) match
        {
            case (v1 : FloatValue, v2 : FloatValue) => new BooleanValue(v1.value != v2.value)
            case (v1 : IntegerValue, v2 : IntegerValue) => new BooleanValue(v1.value != v2.value)
            case (v1 : StringValue, v2 : StringValue) => new BooleanValue(v1.value != v2.value)
            case _  => throw new TypeError( pos, "Arguments to != are not of equal type" )
        }
    }
}


class DynamicASTEvaluator( val context : ValueExecutionContext )
{
    val evaluator = new ValueEvaluator()
    
    def simplify( pos : Position, rawValue : BaseValue ) : BaseValue =
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
                                fn( pos, argList )
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
                                        context.set( p, a )
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
                        case _ => throw new TypeError( pos, "Malformed application" )
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
        paramNames.foreach( param => context.set( param, new UnitValue() ) )
        
        def bindRec( expr : Expression ) : Expression =
        {
            val res = expr match
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
                    val res = context.get(name)
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
                case TypeAnnotation( name, typeNames )              => expr
            }
            
            res.setPos( expr.pos )
            res.exprType = expr.exprType
            
            res
        }
        
        val bound = bindRec( expr )
        
        context.pop()
        
        bound
    }
    
    def eval( expr : Expression ) : BaseValue =
    {
        val pos = expr.pos
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
                    case _                      => throw new TypeError( pos, "Arguments to logical operators must be of boolean type" )
                }
            }

            case LogicalOr( left, right )          =>
            {
                val lvalue = eval(left)
                lvalue match
                {
                    case BooleanValue(true)     => lvalue
                    case BooleanValue(false)    => eval(right)
                    case _                      => throw new TypeError( pos, "Arguments to logical operators must be of boolean type" )
                }
            }
            
            case CmpLt( left, right )               => evaluator.lt( pos, eval(left), eval(right) )
            case CmpLe( left, right )               => evaluator.le( pos, eval(left), eval(right) )
            case CmpGt( left, right )               => evaluator.gt( pos, eval(left), eval(right) )
            case CmpGe( left, right )               => evaluator.ge( pos, eval(left), eval(right) )
            case CmpEq( left, right )               => evaluator.eq( pos, eval(left), eval(right) )
            case CmpNe( left, right )               => evaluator.ne( pos, eval(left), eval(right) )
            
            case Addition( left, right )            => evaluator.add( pos, eval(left), eval(right) )
            case Subtraction( left, right )         => evaluator.subtract( pos, eval(left), eval(right) )
            case Multiplication( left, right )      => evaluator.multiply( pos, eval(left), eval(right) )
            case Division( left, right )            => evaluator.divide( pos, eval(left), eval(right) )
            
            case ListAppend( left, right )          => new ListElementValue( eval(left), eval(right) )
            
            case IdDefinition( name, params, value )  =>
            {
                val rhs = params match
                {
                    case Nil => simplify( pos, eval(value) )
                    case _   => new FunctionValue( params, bindClosureLocals( params, value ) )
                }
                context.set( name, rhs )
                
                rhs
            }
            case IdExpression( name )           => context.get(name).get
            case Apply( lhs, rhs )              =>
            {
                simplify( pos, new ApplicationValue( eval(lhs), eval(rhs) ) )
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
                    case _                      => throw new TypeError( pos, "If expression condition is not of boolean type" )
                }
            }
            
            case TypeAnnotation( name, typeNames )                          => new UnitValue()
            case VariantClauseDefinition( name, elementTypes )              => new UnitValue()
            case VariantTypeDefinition( clauses )                           => new UnitValue()
            case TypeDefinition( typeName, typeParameters, instanceType )   => new UnitValue()
        }
    }
}

