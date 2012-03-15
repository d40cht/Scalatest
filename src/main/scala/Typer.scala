package org.seacourt.pacatoon

import scala.util.parsing.input.Position


object buTypeAST
{
    // Type names - @type foo=double etc
    class TypeNameExecutionContext extends ExecutionContextBase( () => new ContextFrame[ExprType](), "Type not found" )
    {
        set( "float", new TypeFloat() )
        set( "int", new TypeInteger() )
        set( "bool", new TypeBoolean() )
    }
    
    // Types of ids: @def a = 12
    class IdTypeExecutionContext extends ExecutionContextBase( () => new ContextFrame[ExprType](), "Identifier not found" )
    {
    }
    
    
    def apply( expr : Expression )
    {
        def skipTypeRef( exprType : ExprType ) =
        {
            exprType match
            {
                case TypeReference(name, dest)  => dest
                case _                          => exprType
            }
        }
        
        class Worker extends ASTTransformer[Unit](Nil)
        {
            val typeNames = new TypeNameExecutionContext()
            val idTypes = new IdTypeExecutionContext()
            
            def typeUnion( pos : Position, t1 : ExprType, t2 : ExprType ) =
            {
                //if ( t1 != t2 ) throw new TypeError( pos, "Types do not match: %s, %s".format( t1.toString, t2.toString ) )
                t1
            }
            
            override def apply( expr : Expression, continue : () => List[Unit] )
            {
                expr.exprType = expr match
                {
                    case NullExpression()                               => new TypeUnit()
                    case Constant(v)                                    => expr.exprType
                    
                    case BinOpExpression(l, r)                          => continue(); typeUnion( expr.pos, l.exprType, r.exprType )
                
                    case BlockScopeExpression( contents )               =>
                    {
                        typeNames.push()
                        idTypes.push()
                        continue()
                        typeNames.pop()
                        idTypes.pop()
                        
                        contents.exprType
                    }
                    
                    case IdDefinition( id, params, value : Expression ) =>
                    {
                        typeNames.push()
                        idTypes.push()

                        // Is of type function - so we need to look up the parameter types
                        if ( params != Nil )
                        {
                            val idType = idTypes.getOption(id)
                            
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
                        
                        continue()
                        
                        typeNames.pop()
                        idTypes.pop()
                        
                        if ( params == Nil )
                        {
                            idTypes.set( id, value.exprType )
                            value.exprType
                        }
                        else
                        {
                            val idType = idTypes.getOption(id)
                            
                            idType match
                            {
                                case Some(FunctionType( paramTypes, returnType )) => returnType
                                case _ => throw new TypeError( expr.pos, "Type inference failed for: " + id )
                            }
                        }
                    }
                    
                    case TypeAnnotation( name, annotationTypeNames )    =>
                    {
                        typeNames.push()
                        val paramTypes = annotationTypeNames.map( typeName => typeNames.getOption(typeName) match
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
                        
                        continue()
                        
                        new TypeUnit()
                    }
                    
                    
                    case Apply( l, r )                                  =>
                    {
                        continue()
                        l.exprType match
                        {
                            case FunctionType( paramTypes, returnType ) =>
                            {
                                val thisType = paramTypes.head

                                val newFnType = thisType match
                                {
                                    case GenericType(id)    =>
                                    {
                                        def replaceGeneric( x : ExprType ) = if (x==thisType) r.exprType else x
                                        new FunctionType( paramTypes.tail.map( x => replaceGeneric(x) ), replaceGeneric( returnType ) )
                                    }
                                    case _                  =>
                                    {
                                        val forwardedType = thisType.skipTypeRef
                                        
                                        println( r.exprType, forwardedType )
                                        if ( r.exprType != forwardedType ) throw new TypeError( expr.pos, "Invalid function argument" )
                                        else new FunctionType( paramTypes.tail, returnType )
                                    }
                                }
                                
                                newFnType match
                                {
                                    case FunctionType(Nil, returnType)  => newFnType.retType
                                    case _                              => newFnType
                                }
                            }
                            case _ => throw new TypeError( expr.pos, "Function application on invalid type: " + l.exprType.toString )
                        }
                    }
                    case IdExpression( id )                             => idTypes.get( expr.pos, id )
                    case ExprList( elements )                           => continue(); elements.last.exprType
                    
                    case IfExpression( cond, trueBranch, falseBranch )  => continue(); typeUnion( expr.pos, trueBranch.exprType, falseBranch.exprType )
                    
                    case VariantClauseDefinition( name, elementTypeNames )          =>
                    {
                        // TODO: Add constructor fn into var symbols
                        continue();
                        new VariantClauseType( name, elementTypeNames.map( tn => new TypeReference( tn, typeNames.get( expr.pos, tn ) ) ), 0 )
                    }
                    case VariantTypeDefinition( clauses )                           =>
                    {
                        continue();
                        
                        // Remove asInstanceOf vileness
                        val variantType = new VariantType( clauses.map( _.exprType.asInstanceOf[VariantClauseType] ).zipWithIndex.map {
                            // Re-map enums. Also not very nice.
                            case (x, i) =>
                            {
                                new VariantClauseType( x.name, x.elTypes, i )
                            }
                        } )
                        
                        variantType
                    }
                    
                    case TypeDefinition( typeName, typeParameters, instanceType )   =>
                    { 
                        // Add this name as a pending generic type
                        typeNames.set( typeName, new TypeReference(typeName) )
                        
                        // Register any generic parameter names
                        typeNames.push()
                        
                        val paramTypes = typeParameters.map( tn => typeNames.getOption(tn) match
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
                        
                        continue()
                        
                        typeNames.pop()
                        
                        // Get the pending type
                        val pendingType = typeNames.get( expr.pos, typeName ).asInstanceOf[TypeReference]
                        pendingType.destType = instanceType.exprType
                        typeNames.set( typeName, instanceType.exprType )
                        
                        instanceType.exprType match
                        {
                            case VariantType( clauses ) =>
                            {
                                for ( clause <- clauses )
                                {
                                    if ( clause.elTypes == Nil )
                                    {
                                        idTypes.set( clause.name, instanceType.exprType )
                                    }
                                    else
                                    {
                                        idTypes.set( clause.name, new FunctionType( clause.elTypes.map( _.skipTypeRef ), instanceType.exprType ) )
                                    }
                                }
                            }
                            case _ =>
                        }           
                        
                        new TypeUnit()
                    }
                    case _                                              => new TypeUnit()
                }
            }
        }
        
        TransformAST( expr, new Worker() )
    }
}

