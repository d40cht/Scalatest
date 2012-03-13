package org.seacourt.pacatoon

import scala.util.parsing.input.Position


object buTypeAST
{
    // Type names - @type foo=double etc
    class TypeNameExecutionContext extends ExecutionContextBase( () => new ContextFrame[ExprType]() )
    {
        set( "float", new TypeFloat() )
        set( "int", new TypeInteger() )
        set( "bool", new TypeBoolean() )
    }
    
    // Types of ids: @def a = 12
    class IdTypeExecutionContext extends ExecutionContextBase( () => new ContextFrame[ExprType]() )
    {
    }
    
    
    def apply( expr : Expression )
    {
        class Worker extends ASTVisitor
        {
            val typeNames = new TypeNameExecutionContext()
            val idTypes = new IdTypeExecutionContext()
            
            def typeUnion( pos : Position, t1 : ExprType, t2 : ExprType ) =
            {
                //if ( t1 != t2 ) throw new TypeError( pos, "Types do not match: %s, %s".format( t1.toString, t2.toString ) )
                t1
            }
            
            override def before( expr : Expression )
            {
                expr match
                {
                    case BlockScopeExpression( contents )               =>
                    {
                        typeNames.push()
                        idTypes.push()
                    }
                    
                    case IdDefinition( id, params, value : Expression ) =>
                    {
                        typeNames.push()
                        idTypes.push()

                        // Is of type function - so we need to look up the parameter types
                        if ( params != Nil )
                        {
                            val idType = idTypes.get(id)
                            
                            // The visit from here will be into the body of the function, so set param types
                            // up for the call and inference within the implementation.
                            idType match
                            {
                                case Some(FunctionType( paramTypes, returnType )) =>
                                {
                                    for ( (paramName, paramType) <- params.zip( paramTypes ) )
                                    {
                                        idTypes.set( paramName, paramType )
                                    }
                                }
                                case _ => throw new TypeError( expr.pos, "Missing type annotation for function: " + id )
                            }
                        }
                    }
                    
                    case TypeAnnotation( name, annotationTypeNames )    =>
                    {
                        typeNames.push()
                        val paramTypes = annotationTypeNames.map( typeName => typeNames.get(typeName) match
                        {
                            case Some(exprType) => exprType
                            case None =>
                            {
                                // Assume this is a generic type parameter
                                val generic = new GenericType()
                                typeNames.set( typeName, generic )
                                generic
                            }
                        } )
                        val fnType = new FunctionType( paramTypes.dropRight(1), paramTypes.last )
                        
                        typeNames.pop()
                        idTypes.set( name, fnType )
                    }
                    
                    case TypeDefinition( typeName, typeParameters, instanceType )   =>
                    { 
                        // Add this name as a pending generic type
                        typeNames.set( typeName, new WeakTypeReference(typeName) )
                        
                        // Register any generic parameter names
                        typeNames.push()
                        
                        val paramTypes = typeParameters.map( tn => typeNames.get(tn) match
                        {
                            case None =>
                            {
                                // Assume this is a generic type parameter
                                val generic = new GenericType()
                                typeNames.set( tn, generic )
                                generic
                            }
                            case _ =>
                        } )
                    }
                    case _                                              =>
                }
            }
            
            override def after( expr : Expression )
            {
                val newType = expr match
                {
                    case NullExpression()                               => new TypeUnit()
                    case Constant(v)                                    => expr.exprType
                    
                    case BinOpExpression(l, r)                          => typeUnion( expr.pos, l.exprType, r.exprType )
                    
                    case IdDefinition( id, params, value : Expression )   =>
                    {
                        typeNames.pop()
                        idTypes.pop()
                        
                        if ( params == Nil )
                        {
                            idTypes.set( id, value.exprType )
                            value.exprType
                        }
                        else
                        {
                            val idType = idTypes.get(id)
                            
                            idType match
                            {
                                case Some(FunctionType( paramTypes, returnType )) => returnType
                                case _ => throw new TypeError( expr.pos, "Type inference failed for: " + id )
                            }
                        }
                    }    
                    case Apply( l, r )                                  =>
                    {
                        l.exprType match
                        {
                            case FunctionType( paramTypes, returnType ) =>
                            {
                                if ( paramTypes == Nil ) returnType
                                else
                                {
                                    val thisType = paramTypes.head
                                    thisType match
                                    {
                                        case GenericType(id)    =>
                                        {
                                            def replaceGeneric( x : ExprType ) = if (x==thisType) r.exprType else x
                                            new FunctionType( paramTypes.tail.map( x => replaceGeneric(x) ), replaceGeneric( returnType ) )
                                        }
                                        case _                  =>
                                        {
                                            if ( r.exprType != thisType ) throw new TypeError( expr.pos, "Invalid function argument" )
                                            else new FunctionType( paramTypes.tail, returnType )
                                        }
                                    }
                                }
                            }
                            case _ => throw new TypeError( expr.pos, "Function application on invalid type" )
                        }
                    }
                    case IdExpression( id )                             =>
                    {
                        idTypes.get(id) match
                        {
                            case Some(idType)   => idType
                            case None           => throw new TypeError( expr.pos, "Undefined variable: " + id )
                        }
                    }
                    case ExprList( elements )                           => elements.last.exprType
                    case BlockScopeExpression( contents )               =>
                    {
                        typeNames.pop()
                        idTypes.pop()
                        contents.exprType
                    }
                    case IfExpression( cond, trueBranch, falseBranch )  => typeUnion( expr.pos, trueBranch.exprType, falseBranch.exprType )
                    case TypeAnnotation( name, typeNames )              => new TypeUnit()
                    
                    case VariantClauseDefinition( name, elementTypeNames )          =>
                    {
                        // TODO: Add constructor fn into var symbols
                        new VariantClauseType( name, elementTypeNames.map( tn => 
                            typeNames.get(tn) match
                            {
                                case Some(exprType)     => exprType
                                case _                  => throw new TypeError( expr.pos, "Unknown type name " + tn )
                            }
                        ), 0 )
                    }
                    case VariantTypeDefinition( clauses )                           =>
                    {
                        // Remove isInstanceOf vileness
                        new VariantType( clauses.map( _.exprType.asInstanceOf[VariantClauseType] ).zipWithIndex.map {
                            // Re-map enums. Also not very nice.
                            case (x, i ) => new VariantClauseType( x.name, x.elTypes, i )
                        } )
                    }
                    
                    case TypeDefinition( typeName, typeParameters, instanceType )   =>
                    {
                        typeNames.pop()
                        
                        // Get the pending type
                        val pendingType = typeNames.get( typeName ).get.asInstanceOf[WeakTypeReference]
                        pendingType.destType = instanceType.exprType
                        typeNames.set( typeName, instanceType.exprType )                  
                        
                        new TypeUnit()
                    }
                }
                
                expr.exprType = newType
            }
        }
        
        VisitAST( expr, new Worker() )
    }
}

