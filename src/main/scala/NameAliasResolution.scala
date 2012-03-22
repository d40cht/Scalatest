package org.seacourt.pacatoon

import scala.util.parsing.input.Position


class IdentifierStackWithIntrinsics extends ExecutionContextBase( () => new ContextFrame[String, Identifier](), "Identifier not found" )
{
    // Add intrinsic functions to outermost scope
    Intrinsics.members.foreach( m => set( m.name, m.id ) )
}

// Lifts all functions (local/lambda) up to the top level
object NameAliasResolution
{
    class Worker extends ASTTransformer[Expression](new NullExpression())
    {
        val identifierStack = new IdentifierStackWithIntrinsics()
        
        override def apply( expr : Expression, continue : () => List[Expression], rec : Expression => Expression ) : Expression =
        {
            expr match
            {
                case NullExpression()                               => expr
                case ConstantExpression(v)                          => expr
                case BinOpExpression(l, r, opType)                  =>
                {
                    val List( tl, tr ) = continue()
                    new BinOpExpression( tl, tr, opType )
                }
                case ListAppend(l, r)                               =>
                {
                    val List( tl, tr ) = continue()
                    new ListAppend( tl, tr )
                }
                
                case NamedIdDefinition( idName, paramNames, value : Expression ) =>
                {   
                    // If this id is a function, it must already have had a type annotation
                    // so the id must exist                    
                    val theId = if ( paramNames == Nil )
                    {
                        val newId = new Identifier( idName )
                        identifierStack.set( idName, newId )
                        newId
                    }
                    else
                    {
                        identifierStack.getOption( idName ) match
                        {
                            case Some(id)   => id
                            case None       =>
                            {
                                val newId = new Identifier( idName )
                                identifierStack.set( idName, newId )
                                newId
                                //throw new TypeError( expr.pos, "Function definition without type annotation: " + idName )
                            }
                        }
                    }
                    
                    // Note: we registered the id first, in case the function is recursive
                    identifierStack.push()
                    val paramIds = paramNames.map( pn =>
                    {
                        val pId = new Identifier( pn )
                        identifierStack.set( pn, pId )
                        pId
                    } )
                    val List(tv) = continue()
                    identifierStack.pop()
                    
                    new IdDefinition( theId, paramIds, tv )
                }
                
                case NamedIdExpression( idName )                    =>
                {
                    new IdExpression( identifierStack.get( expr.pos, idName ) )
                }
                
                case NamedTypeAnnotation( idName, typeExpr )          =>
                {
                    val theId = new Identifier( idName )
                    identifierStack.set( idName, theId )
                    new TypeAnnotation( theId, typeExpr )
                }
                
                case Apply( l, r )                                  =>
                {
                    val List( tl, tr ) = continue()
                    new Apply( tl, tr )
                }
                
                case ExprList( elements )                           => new ExprList( continue() )
                case BlockScopeExpression( contents )               =>
                {
                    identifierStack.push()
                    val List( transformed ) = continue()
                    identifierStack.pop()
                    new BlockScopeExpression( transformed )
                }
                case IfExpression( cond, trueBranch, falseBranch )  =>
                {
                    val List( tcond, tt, tf ) = continue()
                    new IfExpression( tcond, tt, tf )
                }
                
                case _ => NullExpression()
            }
        }
    }
        
    def apply( expr : Expression ) =
    {
        TransformAST( expr, new Worker() )
    }
}
