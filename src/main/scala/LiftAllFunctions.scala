package org.seacourt.pacatoon

import scala.collection.{mutable, immutable}
import scala.util.parsing.input.Position


// Lifts all functions (local/lambda) up to the top level
object LiftAllFunctions
{
    type FreeVariableMap = immutable.HashMap[Identifier, List[Identifier]]
    
    class Analyser() extends ASTTransformer[Unit](Unit)
    {
        private var fnStack = List[Identifier]()
        private val idFnContext = new ExecutionContextBase( () => new ContextFrame[Identifier, List[Identifier]](), "Identifier not found" )
        
        // Fn -> List of Identifiers that need to be passed in as variables in order to be able to lift
        var fnFreeVariables = new FreeVariableMap()
        
        Intrinsics.members.foreach( m => idFnContext.set( m.id, fnStack ) )
        
        override def apply( expr : Expression, continue : () => List[Unit], rec : Expression => Unit ) : Unit =
        {
            expr match
            {
                case IdDefinition( id, params, value : Expression ) =>
                {
                    idFnContext.set( id, fnStack )
                    
                    fnStack = id :: fnStack
                    params.foreach( pid => idFnContext.set( pid, fnStack ) )
                    continue()
                    fnStack = fnStack.tail
                }
                
                case IdExpression( id )                             =>
                {
                    id.getType match
                    {
                        case TypeFunction( _, _ )   =>
                        case _                      =>
                        {
                            val idContext = idFnContext.get(expr.pos, id)
                            
                            // Functions this variable implicitly traverses through into the current closure context
                            val fnsToUpdate = fnStack.drop( idContext.length )
                            
                            fnsToUpdate.foreach( fId =>
                            {
                                val prev = fnFreeVariables.getOrElse( fId, Nil )
                                fnFreeVariables = fnFreeVariables.updated( fId, id :: prev )
                            } )
                        }
                    }
                }
                
                case _ => continue(); Unit
            }
        }
    }
    
    class Lifter( val freeVarMap : FreeVariableMap )  extends ASTTransformer[Expression](new NullExpression())
    {
        private val idRemapStack = new ExecutionContextBase( () => new ContextFrame[Identifier, Identifier](), "Identifier not found" )
        private var liftedFunctions = immutable.HashMap[Identifier, Identifier]()
        val allFunctions = mutable.ArrayBuffer[IdDefinition]()
        
        private def getPotentiallyRemappedId( id : Identifier ) =
        {
            idRemapStack.getOption(id) match
            {
                case Some(mapId)    => mapId
                case _              => id
            }
        }
        
        override def apply( expr : Expression, continue : () => List[Expression], rec : Expression => Expression ) : Expression =
        {
            val transformedExpr = expr match
            {
                // If this is a reference to a non-function id, check to see if it needs remapping
                // If it's a reference to a function, check if we need to partially apply
                // captured free variables
                case IdExpression( id )                    =>
                {
                    id.getType match
                    {
                        case TypeFunction( paramTypes, retType ) =>
                        {
                            // A function id. Get the lifted function and bind free variables using Apply
                            freeVarMap.get( id ) match
                            {
                                case Some( idsToBind )  => 
                                {
                                    val liftedFnId = liftedFunctions( id )
                                    val remappedArgs = idsToBind.map( x => getPotentiallyRemappedId( x ) )
                                    remappedArgs.foldLeft( new IdExpression( liftedFnId ) : Expression )( (x, y) => new Apply( x, new IdExpression(y) ) )
                                }
                                case _                  => expr
                            }
                        }
                        case _ =>
                        {   
                            // A non function id
                            new IdExpression( getPotentiallyRemappedId( id ) )
                        }
                    }
                }
                
                case IdDefinition( id, params, value : Expression ) =>
                {
                    id.getType match
                    {
                        case TypeFunction( paramTypes, retType ) =>
                        {
                            // This is a function definition. Map the free vars into local param vars and recurse.
                            // Also lift out of local scope into global scope
                            idRemapStack.push()
                            
                            val toCapture = freeVarMap.getOrElse( id, Nil )
                            val newIds = toCapture.map( oldId =>
                            {
                                val newId = new Identifier( oldId.name )
                                newId.setType( oldId.getType )
                                idRemapStack.set( oldId, newId )
                                newId
                            } )
                            
                            val body = rec( value )
                            idRemapStack.pop()
                            
                            val newParams = newIds ++ params
                            val newFnId = new Identifier( id.name )
                            newFnId.setType( new TypeFunction( newParams.map( _.getType ), retType ) )
                            val newFnDefn = new IdDefinition( newFnId, newParams, body )
                            newFnDefn.setPos( expr.pos )
                            
                            allFunctions.append( newFnDefn )
                            
                            liftedFunctions += (id -> newFnId)
                            
                            new NullExpression()
                        }
                        case _ =>
                        {
                            val List( newValue ) = continue()
                            new IdDefinition( id, params, newValue )
                        }
                    }
                }
                
                // Boilerplate shared with NameAliasResolution. Factor out.
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
                
                case Apply( l, r )                                  =>
                {
                    val List( tl, tr ) = continue()
                    new Apply( tl, tr )
                }
                
                case ExprList( elements )                           => new ExprList( continue() )
                case BlockScopeExpression( contents )               =>
                {
                    val List( transformed ) = continue()
                    new BlockScopeExpression( transformed )
                }
                case IfExpression( cond, trueBranch, falseBranch )  =>
                {
                    val List( tcond, tt, tf ) = continue()
                    new IfExpression( tcond, tt, tf )
                }
                
                case _ => NullExpression()
            }
            transformedExpr.pos = expr.pos
            transformedExpr
        }
    }
        
    def apply( expr : Expression ) =
    {
        val freeVarAnalyser = new Analyser()
        TransformAST( expr, freeVarAnalyser )
        
        val freeVars = freeVarAnalyser.fnFreeVariables
        freeVars.foreach( f => println( f._1 + ": " + f._2.map( _.toString ).mkString(", ") ) )
        
        val lifter = new Lifter( freeVars )
        val lifted = TransformAST( expr, lifter )
        
        val newCompilationUnit = new ExprList( lifter.allFunctions.toList ++ List(lifted) )
        newCompilationUnit.setType( expr.getType )
        
        newCompilationUnit
    }
}
