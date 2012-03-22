package org.seacourt.pacatoon

import scala.util.parsing.input.Position


object buTypeAST
{
    // Type names - @type foo=double etc
    class TypeNameExecutionContext extends ExecutionContextBase( () => new ContextFrame[ExprType](), "Type not found" )
    {
        set( "float", TypeFloat )
        set( "int", TypeInteger )
        set( "bool", TypeBoolean )
    }
    
    // Types of ids: @def a = 12
    class IdTypeExecutionContext extends ExecutionContextBase( () => new ContextFrame[ExprType](), "Identifier not found" )
    {
        set( "nil", new TypeList( new TypeGeneric() ) )
        set( "toString", new TypeFunction( List( new TypeGeneric() ), TypeString ) )
        set( "print", new TypeFunction( List( TypeString ), TypeUnit ) )
        
        {
            val elType = new TypeGeneric()
            set( "head", new TypeFunction( List( new TypeList(elType) ), elType ) )
        }
        
        {
            val listType = new TypeList( new TypeGeneric() )
            set( "tail", new TypeFunction( List( listType ), listType ) )
        }
        
        {
            val comparisonType = new TypeGeneric()
            set( "assertEqual", new TypeFunction( List( comparisonType, comparisonType ), TypeUnit ) )
        }
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
            
            def typeUnion( pos : Position, t1 : ExprType, t2 : ExprType ) : ExprType =
            {
                (t1, t2) match
                {
                    case (TypeList(x), TypeList(y)) => typeUnion(pos, x, y); x;
                    case (x, TypeGeneric(_))        => x
                    case (TypeGeneric(_), x)        => x
                    case _                          =>
                    {
                        if ( t1 != t2 ) throw new TypeError( pos, "Types do not match: %s, %s".format( t1.toString, t2.toString ) )
                        t1
                    }
                }
            }
            
            override def apply( expr : Expression, continue : () => List[Unit], rec : Expression => Unit )
            {
                val exprType = expr match
                {
                    case NullExpression()                               => TypeUnit
                    case ConstantExpression(v)                          => expr.getType
                    
                    case ListAppend(l, r)                               =>
                    {
                        continue()
                        (l.getType, r.getType) match
                        {
                            case (x, TypeList(y))   =>
                            {
                                new TypeList(typeUnion(expr.pos, x, y))
                            }
                            case _                  => throw new TypeError( expr.pos, "Type mismatch in list append" )
                        }
                    }
                    
                    case BinOpExpression(l, r, optType)                 => continue(); typeUnion( expr.pos, l.getType, r.getType )
                
                    case BlockScopeExpression( contents )               =>
                    {
                        typeNames.push()
                        continue()
                        typeNames.pop()
                        
                        contents.getType
                    }
                    
                    case IdDefinition( id, params, value : Expression ) =>
                    {
                        typeNames.push()

                        // Is of type function - so we need to look up the parameter types
                        if ( params != Nil )
                        {
                            val idType = id.getType
                            
                            // The visit from here will be into the body of the function, so set param types
                            // up for the call and inference within the implementation.
                            idType match
                            {
                                case TypeFunction( paramTypes, returnType ) =>
                                {
                                    for ( (paramId, paramType) <- params.zip( paramTypes ) )
                                    {
                                        paramId.setType( paramType )
                                    }
                                }
                                case _ => throw new TypeError( expr.pos, "Missing type annotation for function: " + id.toString )
                            }
                        }
                        
                        continue()
                        
                        typeNames.pop()
                        
                        if ( params == Nil )
                        {
                            //println( "Setting " + id + " to " + value.getType )
                            id.setType( value.getType )
                            value.getType
                        }
                        else
                        {
                            val idType = id.getType
                            
                            idType match
                            {
                                case TypeFunction( paramTypes, returnType ) => returnType
                                case _ => throw new TypeError( expr.pos, "Type inference failed for: " + id )
                            }
                        }
                    }
                    
                    
                    case IdExpression( id )                             => id.getType

                    case NamedTypeExpr( typeName )                      =>
                    {
                        typeNames.getOption(typeName) match
                        {
                            case Some(exprType) => exprType
                            case None =>
                            {
                                // Assume this is a generic type parameter
                                val generic = new TypeGeneric()
                                typeNames.set( typeName, generic )
                                generic
                            }
                        }
                    }
                    
                    case TypeListExpr( expr )                           =>
                    {
                        continue()
                        new TypeList( expr.getType )
                    }
                    
                    case TypeExpr( elements )                           =>
                    {
                        continue()
                        val elTypes = elements.map( _.getType )
                        elTypes match
                        {
                            case List(singleType)   => singleType
                            case _                  =>
                            {
                                new TypeFunction( elTypes.dropRight(1), elTypes.last )
                            }
                        }             
                    }
                    
                    case TypeAnnotation( id, theType )    =>
                    {
                        typeNames.push()
                        continue()
                        typeNames.pop()
                        //println( "Annotating type: " + name + ", " + theType.getType )
                        id.setType( theType.getType )
                        TypeUnit
                    }
                    
                    
                    case Apply( l, r )                                  =>
                    {
                        continue()
                        
                        expr.getType match
                        {
                            case TypeFunction( paramTypes, returnType ) =>
                            {
                                val thisType = paramTypes.head
                                
                                // Build a map of bound generic types and check the supplied argument is of
                                // compatible type for the parameter
                                var typeMap = Map[ExprType, ExprType]()
                                def subGenerics( paramType : ExprType, concreteType : ExprType )
                                {
                                    (paramType, concreteType) match
                                    {
                                        case (TypeGeneric(id), _)       =>
                                        {
                                            typeMap.get(paramType) match
                                            {
                                                case Some(t)    => t
                                                case _          =>
                                                {
                                                    typeMap += paramType -> concreteType
                                                }
                                            }
                                        }
                                        case (TypeList(x), TypeList(y)) => subGenerics( x, y )
                                        case (TypeFunction(params1, ret1), TypeFunction(params2, ret2)) =>
                                        {
                                            (params1 zip params2).map { case (x, y) => subGenerics(x, y) }
                                            subGenerics( ret1, ret2 )
                                        }
                                        case (x, y) => if ( x != y ) throw new TypeError( expr.pos, "Invalid types for application: " + x + ", " + y )
                                    }
                                }
                                
                                subGenerics( thisType, r.getType )
                                
                                def transformFn( fType : ExprType ) : ExprType =
                                {
                                    fType match
                                    {
                                        case TypeGeneric(id)            => typeMap.getOrElse( fType, fType )
                                        case TypeList(elType)           => new TypeList( transformFn(elType) )
                                        case TypeFunction(params, ret)  => new TypeFunction( params.map( x => transformFn(x) ), transformFn(ret) )
                                        case _                          => fType
                                    }
                                }
                                
                                val newFnType = transformFn( new TypeFunction( paramTypes.tail, returnType ) )                                
                                newFnType match
                                {
                                    case TypeFunction( List(), retType )    => retType
                                    case TypeFunction( params, retType )    => new TypeFunction( params, retType )
                                    case x  => throw new TypeError( expr.pos, "Invalid type for application: " + x )
                                }
                            }
                            case _ => throw new TypeError( expr.pos, "Function application on invalid type: " + l.getType.toString )
                        }
                    }
                    
                    case ExprList( elements )                           => continue(); elements.last.getType
                    
                    case IfExpression( cond, trueBranch, falseBranch )  => continue(); typeUnion( expr.pos, trueBranch.getType, falseBranch.getType )
                    
                    case VariantClauseDefinition( name, elementTypeNames )          =>
                    {
                        // TODO: Add constructor fn into var symbols
                        continue();
                        new TypeVariantClause( name, elementTypeNames.map( tn => new TypeReference( tn, typeNames.get( expr.pos, tn ) ) ), 0 )
                    }
                    case TypeVariantDefinition( clauses )                           =>
                    {
                        continue();
                        
                        // Remove asInstanceOf vileness
                        val variantType = new TypeVariant( clauses.map( _.getType.asInstanceOf[TypeVariantClause] ).zipWithIndex.map {
                            // Re-map enums. Also not very nice.
                            case (x, i) =>
                            {
                                new TypeVariantClause( x.name, x.elTypes, i )
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
                                val generic = new TypeGeneric()
                                typeNames.set( tn, generic )
                                generic
                            }
                            case _ =>
                        } )
                        
                        continue()
                        
                        typeNames.pop()
                        
                        // Get the pending type
                        val pendingType = typeNames.get( expr.pos, typeName ).asInstanceOf[TypeReference]
                        pendingType.destType = instanceType.getType
                        typeNames.set( typeName, instanceType.getType )
                        
                        // If it's a variant type defn, add the constructors to the local scope
                        /*instanceType.getType match
                        {
                            case TypeVariant( clauses ) =>
                            {
                                for ( clause <- clauses )
                                {
                                    if ( clause.elTypes == Nil )
                                    {
                                        idTypes.set( clause.name, instanceType.getType )
                                    }
                                    else
                                    {
                                        idTypes.set( clause.name, new TypeFunction( clause.elTypes.map( _.skipTypeRef ), instanceType.getType ) )
                                    }
                                }
                            }
                            case _ =>
                        }*/      
                        
                        TypeUnit
                    }
                    case _                                              => TypeUnit
                }
            }
        }
        
        TransformAST( expr, new Worker() )
    }
}

