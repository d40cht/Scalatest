package org.seacourt.pacatoon

import scala.util.parsing.input.Position


// Lifts all functions (local/lambda) up to the top level
object LiftAllFunctions
{
    class Worker extends ASTTransformer[Expression](new NullExpression())
    {
        var nameStack = List[String]()
        
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
                
                case IdDefinition( id, params, value : Expression ) =>
                {
                    nameStack = id :: nameStack
                    val List(tv) = continue()
                    nameStack = nameStack.tail
                    if ( params == Nil )
                    {
                        new IdDefinition( id, params, tv )
                    }
                    else
                    {
                        // TODO
                        new IdDefinition( id, params, tv )
                    }
                    
                }
                case Apply( l, r )                                  =>
                {
                    val List( tl, tr ) = continue()
                    new Apply( tl, tr )
                }
                case IdExpression( id )                             =>
                {
                    // TODO: What scope does this id come from?
                    expr
                }
                case ExprList( elements )                           => new ExprList( continue() )
                case BlockScopeExpression( contents )               => new BlockScopeExpression( continue().head )
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
