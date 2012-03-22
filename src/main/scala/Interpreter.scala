package org.seacourt.pacatoon


import scala.collection.{mutable, immutable}
import scala.util.parsing.input.{Position, NoPosition}

sealed abstract class BaseValue
{
    def toString : String
}

case class UnitValue() extends BaseValue
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

case class IntegerValue( val value : Int ) extends BaseValue
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

case class FunctionValue( params : List[Identifier], body : Expression ) extends BaseValue
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

class VariantValue( val els : List[BaseValue] ) extends BaseValue
{
    override def toString = "Variant: " + els
}



class PositionedException( position : Position, msg : String ) extends RuntimeException( "("+position.line+", "+position.column+"): " + msg )

class TypeError( _position : Position, _msg : String ) extends PositionedException(_position, _msg)
class VariableNotFoundError( _position : Position, _msg : String ) extends PositionedException(_position, _msg)
class AssertionFailure( _position : Position, _msg : String ) extends PositionedException(_position, _msg)



class ValueExecutionContext extends ExecutionContextBase( () => new ContextFrame[Identifier, BaseValue](), "Identifier not found" )
{
    Intrinsics.members.foreach( m => set( m.id, m.value ) )
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
    def bindClosureLocals( params : List[Identifier], expr : Expression ) : Expression =
    {
        context.push()
        // Any variables with the same name as function params must not be bound to outer scopes
        params.foreach( param => context.set( param, new UnitValue() ) )
        
        def bindRec( expr : Expression ) : Expression =
        {
            val res = expr match
            {
                case NullExpression()                   => new NullExpression()
                case ConstantExpression( value )        => expr
                
                case BinOpExpression( left, right, op ) => new BinOpExpression( bindRec(left), bindRec(right), op )
                case ListAppend( left, right )          => new ListAppend( bindRec(left), bindRec(right) )
                
                case IdDefinition( id, args, value )  => new IdDefinition( id, args, bindRec( value ) )
                case IdExpression( id )               =>
                {
                    val res = context.getOption(id)
                    res match
                    {
                        case None               => expr
                        case Some(UnitValue())  => expr
                        case Some(v)            => new ConstantExpression(v)
                    }
                }
                case Apply( lhs, rhs )                  => new Apply( bindRec( lhs ), bindRec( rhs ) )
                case ExprList( elements )               => new ExprList( elements.map( x => bindRec(x) ) )
                case BlockScopeExpression( contents )   => 
                {
                    val res = bindRec( contents )
                    
                    new BlockScopeExpression( res )
                }
                case IfExpression( cond, trueBranch, falseBranch )  => new IfExpression( bindRec(cond), bindRec(trueBranch), bindRec(falseBranch) )
                case NamedTypeAnnotation( name, typeNames )              => expr
                case _ => expr
            }
            
            res.setPos( expr.pos )
            res.setType( expr.getType )
            
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
            case ConstantExpression( value )        => value
            
            case BinOpExpression( left, right, op ) => op match
            {
                case BinOpType.LogicalAnd          =>
                {
                    val lvalue = eval(left)
                    lvalue match
                    {
                        case BooleanValue(false)    => lvalue
                        case BooleanValue(true)     => eval(right)
                        case _                      => throw new TypeError( pos, "Arguments to logical operators must be of boolean type" )
                    }
                }

                case BinOpType.LogicalOr          =>
                {
                    val lvalue = eval(left)
                    lvalue match
                    {
                        case BooleanValue(true)     => lvalue
                        case BooleanValue(false)    => eval(right)
                        case _                      => throw new TypeError( pos, "Arguments to logical operators must be of boolean type" )
                    }
                }
                
                case BinOpType.CmpLt               => evaluator.lt( pos, eval(left), eval(right) )
                case BinOpType.CmpLe               => evaluator.le( pos, eval(left), eval(right) )
                case BinOpType.CmpGt               => evaluator.gt( pos, eval(left), eval(right) )
                case BinOpType.CmpGe               => evaluator.ge( pos, eval(left), eval(right) )
                case BinOpType.CmpEq               => evaluator.eq( pos, eval(left), eval(right) )
                case BinOpType.CmpNe               => evaluator.ne( pos, eval(left), eval(right) )
                
                case BinOpType.Addition            => evaluator.add( pos, eval(left), eval(right) )
                case BinOpType.Subtraction         => evaluator.subtract( pos, eval(left), eval(right) )
                case BinOpType.Multiplication      => evaluator.multiply( pos, eval(left), eval(right) )
                case BinOpType.Division            => evaluator.divide( pos, eval(left), eval(right) )
            }
            
            case ListAppend( left, right )          => new ListElementValue( eval(left), eval(right) )
            
            case IdDefinition( id, params, value )  =>
            {
                val rhs = params match
                {
                    case Nil => simplify( pos, eval(value) )
                    case _   => new FunctionValue( params, bindClosureLocals( params, value ) )
                }
                context.set( id, rhs )
                
                rhs
            }
            case IdExpression( id )           => context.get(pos, id)
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
            
            /*case NamedTypeAnnotation( name, typeNames )                          => new UnitValue()
            
            // Build a function to generate a VariantValue
            case VariantClauseDefinition( name, elementTypes )              =>
            {
                val numParams = elementTypes.length
                context.set( name, new BuiltInFunction( numParams, (pos, args) =>
                {
                    if ( args.length != numParams ) throw new TypeError( pos, "Variant ctor " + name + " takes " + numParams + " arguments" )
                    new VariantValue( args )
                } ) )
                
                new UnitValue()
            }
            case TypeVariantDefinition( clauses )                           =>
            {
                clauses.map( x => eval(x) )
                new UnitValue()
            }
            case TypeDefinition( typeName, typeParameters, instanceType )   =>
            {
                eval(instanceType)
                new UnitValue()
            }*/
            
            case _ => new UnitValue()
        }
    }
}

