package org.seacourt.pacatoon

import scala.util.parsing.input.Positional


sealed abstract class Expression extends Positional
{
    var exprType : ExprType = TypeNone
}

case class NullExpression extends Expression
case class ConstantExpression( value : BaseValue ) extends Expression

object BinOpType extends Enumeration
{
    val LogicalAnd      = Value("LogicalAnd")
    val LogicalOr       = Value("LogicalOr")
    val CmpLt           = Value("CmpLt")
    val CmpLe           = Value("CmpLe")
    val CmpGt           = Value("CmpGt")
    val CmpGe           = Value("CmpGe")
    val CmpEq           = Value("CmpEq")
    val CmpNe           = Value("CmpNe")
    val Addition        = Value("Addition")
    val Subtraction     = Value("Subtraction")
    val Multiplication  = Value("Multiplication")
    val Division        = Value("Division")
}

case class BinOpExpression( left : Expression, right : Expression, opType : BinOpType.Value ) extends Expression

case class ListAppend( left : Expression, right : Expression ) extends Expression

case class IdDefinition( id : String, params : List[String], value : Expression ) extends Expression
case class Apply( lhs : Expression, rhs : Expression ) extends Expression
case class IdExpression( id : String ) extends Expression
case class ExprList( val elements : List[Expression] ) extends Expression
case class BlockScopeExpression( val contents : Expression ) extends Expression
case class IfExpression( val cond : Expression, val trueBranch : Expression, val falseBranch : Expression ) extends Expression

case class NamedTypeExpr( val typeName : String ) extends Expression
case class TypeListExpr( val elExpr : Expression ) extends Expression
case class TypeExpr( val elements : List[Expression] ) extends Expression

case class TypeAnnotation( val name : String, val theType : Expression ) extends Expression

case class VariantClauseDefinition( clauseName : String, elementTypeNames : List[String] ) extends Expression
case class TypeVariantDefinition( clauses : List[VariantClauseDefinition] ) extends Expression
case class TypeDefinition( typeName : String, typeParameters : List[String], instanceType : TypeVariantDefinition ) extends Expression


abstract class ASTTransformer[T]( val default : T )
{
    def apply( expr : Expression, continue : () => List[T], rec : Expression => T ) : T =
    {
        continue()
        default
    }
}

object TransformAST
{
    def apply[T]( expr : Expression, transformer : ASTTransformer[T] ) =
    {
        def rec( expr : Expression ) : T =
        {
            expr match
            {
                case NullExpression()                               => transformer( expr, () => Nil, rec )
                case ConstantExpression(v)                          => transformer( expr, () => Nil, rec )
                case BinOpExpression(l, r, opType)                  => transformer( expr, () => List( rec(l), rec(r) ), rec )
                case ListAppend(l, r)                               => transformer( expr, () => List( rec(l), rec(r) ), rec )
                
                case IdDefinition( id, params, value : Expression ) => transformer( expr, () => List( rec(value) ), rec )
                case Apply( l, r )                                  => transformer( expr, () => List( rec(l), rec(r) ), rec )
                case IdExpression( id )                             => transformer( expr, () => Nil, rec )
                case ExprList( elements )                           => transformer( expr, () => elements.map( x => rec(x) ), rec )
                case BlockScopeExpression( contents )               => transformer( expr, () => List( rec(contents) ), rec )
                case IfExpression( cond, trueBranch, falseBranch )  => transformer( expr, () => List( rec(cond), rec(trueBranch), rec(falseBranch) ), rec )
                case NamedTypeExpr( name )                                      => transformer( expr, () => Nil, rec )
                case TypeListExpr( elExpr )                                     => transformer( expr, () => List( rec(elExpr) ), rec )
                case TypeExpr( elements )                                       => transformer( expr, () => elements.map( c => rec(c) ), rec )
                case TypeAnnotation( name, theType )                            => transformer( expr, () => List ( rec(theType) ), rec )
                case VariantClauseDefinition( name, elementTypes )              => transformer( expr, () => Nil, rec )
                case TypeVariantDefinition( clauses )                           => transformer( expr, () => clauses.map( c => rec(c) ), rec )
                case TypeDefinition( typeName, typeParameters, instanceType )   => transformer( expr, () => List( rec(instanceType) ), rec )
            }
        }
        
        rec( expr )
    }
}

object DumpAST
{
    def apply( expr : Expression )
    {
        class Dumper extends ASTTransformer[Unit]( Nil )
        {
            var indent = 0
            
            override def apply( expr : Expression, continue : () => List[Unit], rec : Expression => Unit )
            {
                def pr( s : String ) = println( ("| "*indent) + s )// + " : " + expr.exprType.toString )
                
                indent += 1
                expr match
                {
                    case NullExpression()                               => pr( "Null" )
                    case ConstantExpression(v)                          => pr( "Constant: " + v.toString )
                    
                    case BinOpExpression(l, r, opType)                  => pr( "BinOp: " + opType.toString ); continue();
                    
                    case ListAppend(l, r)                               => pr( "ListAppend" ); continue();
                    
                    case IdDefinition( id, params, value : Expression ) =>
                    {
                        pr( "IdDefinition " + id )
                        continue()
                        DumpTypes( value.exprType )
                    }
                    case Apply( l, r )                                  => pr( "Apply" ); continue();
                    case IdExpression( id )                             => pr( "Id: " + id )
                    case ExprList( elements )                           => pr( "ExprList" ); continue();
                    case BlockScopeExpression( contents )               => pr( "BlockScope" ); continue();
                    case IfExpression( cond, trueBranch, falseBranch )  => pr( "IfExpression" ); continue();
                    
                    // The below are all type-related and will not appear in any AST sent out for compilation
                    case NamedTypeExpr( typeName )                                  => pr( "Named type " + typeName )
                    case TypeListExpr( expr )                                       => pr( "List type " ); continue();
                    case TypeExpr( elements )                                       => pr( "Type expr" ); continue();
                    case TypeAnnotation( name, typeExpr )                           => pr( "TypeAnnotation " + name ); continue();
                    
                    case VariantClauseDefinition( name, elementTypes )              => pr( "VariantClause " + name + " : " + elementTypes.mkString( " " ) ); continue();
                    case TypeVariantDefinition( clauses )                           => pr( "TypeVariantDefinition" ); continue();
                    case TypeDefinition( typeName, typeParameters, instanceType )   => pr( "TypeDefinition " + typeName + " : " + typeParameters.mkString( " " ) ); continue();
                }
                indent -= 1
            }
        }
        
        TransformAST( expr, new Dumper() )
    }
}
