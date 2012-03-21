package org.seacourt.pacatoon.bytecode

import org.seacourt.pacatoon._

import scala.util.parsing.input.{Position, Positional}

import scala.collection.{mutable, immutable}

object ValueReference
{
    private var lastId = 0
    
    def nextId =
    {
        val curr = lastId
        lastId += 1
        curr
    }
}

final class ValueReference( val name : String )
{
    val id = ValueReference.nextId.toString
    override def toString = name + "_" + id
}

sealed abstract class ByteCode
{
    var next : Option[ByteCode] = None
}


case class Label() extends ByteCode
case class Definition( val target : ValueReference ) extends ByteCode
case class Branch( val target : ValueReference ) extends ByteCode
case class Constant( val value : BaseValue ) extends ByteCode
case class ListAppend( val lhs : ValueReference, val rhs : ValueReference ) extends ByteCode
case class ConditionalBranch( val cond : ValueReference, val thenBranch : ValueReference, val elseBranch : ValueReference ) extends ByteCode
case class BinOp( val lhsVar : ValueReference, val rhsVar : ValueReference, val opType : BinOpType.Value ) extends ByteCode

case class Phi( val inputs : List[ValueReference] ) extends ByteCode

class CodeGenError( _position : Position, _msg : String ) extends PositionedException(_position, _msg)

object ByteCodeGenerator
{
    class IdContext extends ExecutionContextBase( () => new ContextFrame[ValueReference](), "Identifier not found" )
    {
    }
    
    class CodeGenerator
    {
        private var lastId = 0
        val idContext = new IdContext()
        
        // Register the value
        def register( value : ValueReference, instr : ByteCode ) : ValueReference =
        {
            //functionStack.head._2.append( instr )
            
            val name = value.toString
            val padding = " " * (30-name.length)
            println( name + padding + " : " + instr.toString )
            value
        }
        
        /*var functionStack = ("Global", mutable.ArrayBuffer[ByteCode]())::Nil
        
        def pushFunction( name : String )
        {
            functionStack = (name, mutable.ArrayBuffer[ByteCode]()) :: functionStack
            idContext.push()
        }
        
        def popFunction()
        {
            val head = functionStack.head
            idContext.pop()
            functionStack = functionStack.tail
        }*/
    }
    
    class Worker extends ASTTransformer[ValueReference]( new ValueReference("Null value") )
    {
        val codeGenerator = new CodeGenerator()
        
        override def apply( expr : Expression, continue : () => List[ValueReference], rec : Expression => ValueReference ) : ValueReference =
        {
            if ( expr.exprType == TypeNone ) throw new CodeGenError( expr.pos, "Untyped AST node in codegen: " + expr.getClass.getName )
            
            expr match
            {
                case ConstantExpression( v ) => codeGenerator.register( new ValueReference("const"), new Constant( v ) )
                
                case BinOpExpression( l, r, opType ) =>
                {
                    import BinOpType._
                    
                    opType match
                    {
                        // LogicalAnd and LogicalOr should be transformed into if expression
                        case (LogicalAnd | LogicalOr)   => throw new CodeGenError( expr.pos, "Invalid primitive in code generation: " + opType )
                        case _                          =>
                        {
                            val List(lVal, rVal) = continue()
                            codeGenerator.register( new ValueReference("binop" + opType.toString), new BinOp( lVal, rVal, opType ) )
                        }
                    }
                }
                
                case IdDefinition( id, params, value : Expression ) =>
                {
                    if ( params != Nil )
                    {
                        throw new CodeGenError( expr.pos, "Function definition not yet supported " )
                        //codeGenerator.pushFunction( id )
                        //continue()
                        //codeGenerator.popFunction()
                    }
                    else
                    {
                        val List(exprValue) = continue()
                        val idRef = new ValueReference(id)
                        codeGenerator.idContext.set( id, idRef )
                        codeGenerator.register( idRef, new Definition(exprValue) )
                    }
                }
                
                case Apply( l, r ) => throw new CodeGenError( expr.pos, "Application not yet supported " )
                case IdExpression( id ) =>
                {
                    codeGenerator.idContext.get( expr.pos, id )
                }
                
                case ExprList( elements ) => continue().last
                case BlockScopeExpression( contents ) =>
                {
                    codeGenerator.idContext.push()
                    val last = continue().head
                    codeGenerator.idContext.pop()
                    last
                }
                
                case IfExpression( cond, thenBranch, elseBranch ) =>
                {
                    val condValue = rec( cond )
                    
                    val thenBranchValue = new ValueReference("thenLabel")
                    val elseBranchValue = new ValueReference("elseLabel")
                    val phiValue = new ValueReference("ifPhi")
                    
                    codeGenerator.register( new ValueReference("ifbranch"), new ConditionalBranch( condValue, thenBranchValue, elseBranchValue ) )
                    
                    codeGenerator.register( thenBranchValue, new Label() )
                    val thenFinalValue = rec( thenBranch )
                    codeGenerator.register( new ValueReference("exit"), new Branch( phiValue ) )
                    
                    codeGenerator.register( elseBranchValue, new Label() )
                    val elseFinalValue = rec( elseBranch )
                    codeGenerator.register( new ValueReference("exit"), new Branch( phiValue ) )
                    
                    codeGenerator.register( phiValue, new Phi( List(thenFinalValue, elseFinalValue) ) )
                }
                
                case _ => throw new CodeGenError( expr.pos, "Invalid AST node for codegen: " + expr.getClass.getName )
            }
        }
    }
    
    def apply( start : Expression )
    {
        TransformAST( start, new Worker() )
    }
}


