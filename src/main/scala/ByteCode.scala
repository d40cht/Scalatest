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
    val id = ValueReference.nextId
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

case class Phi() extends ByteCode
{
    private val inputs = mutable.ArrayBuffer[ValueReference]()
    def addInput( input : ValueReference )
    {
        inputs.append( input )
    }
}

class CodeGenError( _position : Position, _msg : String ) extends PositionedException(_position, _msg)

object ByteCodeGenerator
{
    class CodeGenerator
    {
        private var lastId = 0
        
        // Register the value
        def register( value : ValueReference, instr : ByteCode, append : Boolean = true ) : ValueReference =
        {
            value
        }
        
        // Push back instruction
        def append( instr : ByteCode )
        {
        }
        
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
                    
                    continue()
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
                    if ( params != Nil ) throw new CodeGenError( expr.pos, "Function definitions not yet supported" )
                    
                    val List(exprValue) = continue()
                    
                    codeGenerator.register( new ValueReference(id), new Definition(exprValue) )
                }
                
                case Apply( l, r ) => throw new CodeGenError( expr.pos, "Application not yet supported " )
                case IdExpression( id ) =>
                {
                    codeGenerator.register( new ValueReference( "idref" ), new Definition( new ValueReference( id ) ) )
                }
                
                case ExprList( elements ) => continue().last
                case BlockScopeExpression( contents ) => continue().head
                
                case IfExpression( cond, thenBranch, elseBranch ) =>
                {
                    val condValue = rec( cond )
                    
                    val thenBranchLabel = new Label()
                    val elseBranchLabel = new Label()
                    val thenBranchValue = codeGenerator.register( new ValueReference("thenLabel"), thenBranchLabel, false )
                    val elseBranchValue = codeGenerator.register( new ValueReference("elseLabel"), elseBranchLabel, false )
                    
                    val phi = new Phi()
                    val phiValue = codeGenerator.register( new ValueReference("ifphi"), phi, false )
                    
                    codeGenerator.register( new ValueReference("ifbranch"), new ConditionalBranch( condValue, thenBranchValue, elseBranchValue ) )
                    
                    codeGenerator.append( thenBranchLabel )
                    val thenFinalValue = rec( thenBranch )
                    
                    codeGenerator.append( elseBranchLabel )
                    val elseFinalValue = rec( elseBranch )
                    
                    phi.addInput( thenFinalValue )
                    phi.addInput( elseFinalValue )
                    
                    codeGenerator.append( phi )
                    
                    phiValue
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


