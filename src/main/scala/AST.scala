package org.seacourt.pacatoon

import scala.util.parsing.input.Positional


sealed abstract class Expression extends Positional
{
    var exprType : ExprType = new Untyped()
}

case class NullExpression extends Expression
case class Constant( value : BaseValue ) extends Expression


case class BinOpExpression( left : Expression, right : Expression ) extends Expression

case class LogicalAnd( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class LogicalOr( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpLt( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpLe( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpGt( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpGe( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpEq( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class CmpNe( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Addition( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Subtraction( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Multiplication( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Division( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )

case class ListAppend( left : Expression, right : Expression ) extends Expression

case class IdDefinition( id : String, params : List[String], value : Expression ) extends Expression
case class Apply( lhs : Expression, rhs : Expression ) extends Expression
case class IdExpression( id : String ) extends Expression
case class ExprList( val elements : List[Expression] ) extends Expression
case class BlockScopeExpression( val contents : Expression ) extends Expression
case class IfExpression( val cond : Expression, val trueBranch : Expression, val falseBranch : Expression ) extends Expression

case class NamedTypeExpr( val typeName : String ) extends Expression
case class ListTypeExpr( val elExpr : Expression ) extends Expression
case class TypeExpr( val elements : List[Expression] ) extends Expression

case class TypeAnnotation( val name : String, val theType : Expression ) extends Expression

case class VariantClauseDefinition( clauseName : String, elementTypeNames : List[String] ) extends Expression
case class VariantTypeDefinition( clauses : List[VariantClauseDefinition] ) extends Expression
case class TypeDefinition( typeName : String, typeParameters : List[String], instanceType : VariantTypeDefinition ) extends Expression


abstract class ASTTransformer[T]( val default : T )
{
    def apply( expr : Expression, continue : () => List[T] ) : T =
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
                case NullExpression()                               => transformer( expr, () => Nil)
                case Constant(v)                                    => transformer( expr, () => Nil)
                case BinOpExpression(l, r)                          => transformer( expr, () => List( rec(l), rec(r) ) )
                case ListAppend(l, r)                               => transformer( expr, () => List( rec(l), rec(r) ) )
                
                case IdDefinition( id, params, value : Expression ) => transformer( expr, () => List( rec(value) ) )
                case Apply( l, r )                                  => transformer( expr, () => List( rec(l), rec(r) ) )
                case IdExpression( id )                             => transformer( expr, () => Nil )
                case ExprList( elements )                           => transformer( expr, () => elements.map( x => rec(x) ) )
                case BlockScopeExpression( contents )               => transformer( expr, () => List( rec(contents) ) )
                case IfExpression( cond, trueBranch, falseBranch )  => transformer( expr, () => List( rec(cond), rec(trueBranch), rec(falseBranch) ) )
                case NamedTypeExpr( name )                                      => transformer( expr, () => Nil)
                case ListTypeExpr( elExpr )                                     => transformer( expr, () => List( rec(elExpr) ) )
                case TypeExpr( elements )                                       => transformer( expr, () => elements.map( c => rec(c) ) )
                case TypeAnnotation( name, theType )                            => transformer( expr, () => List ( rec(theType) ) )
                case VariantClauseDefinition( name, elementTypes )              => transformer( expr, () => Nil)
                case VariantTypeDefinition( clauses )                           => transformer( expr, () => clauses.map( c => rec(c) ) )
                case TypeDefinition( typeName, typeParameters, instanceType )   => transformer( expr, () => List( rec(instanceType) ) )
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
            
            override def apply( expr : Expression, continue : () => List[Unit] )
            {
                def pr( s : String ) = println( ("| "*indent) + s )// + " : " + expr.exprType.toString )
                
                indent += 1
                expr match
                {
                    case NullExpression()                               => pr( "Null" )
                    case Constant(v)                                    => pr( "Constant: " + v.toString )
                    case LogicalAnd(l, r)                               => pr( "LogicalAnd" ); continue();
                    case LogicalOr(l, r)                                => pr( "Division" ); continue();
                    case CmpLt(l, r)                                    => pr( "CmpLt" ); continue();
                    case CmpLe(l, r)                                    => pr( "CmpLe" ); continue();
                    case CmpGt(l, r)                                    => pr( "CmpGt" ); continue();
                    case CmpGe(l, r)                                    => pr( "CmpGe" ); continue();
                    case CmpEq(l, r)                                    => pr( "CmpEq" ); continue();
                    case CmpNe(l, r)                                    => pr( "CmpNe" ); continue();
                    
                    case ListAppend(l, r)                               => pr( "ListAppend" ); continue();
                    case Addition(l, r)                                 => pr( "Addition" ); continue();
                    case Subtraction(l, r)                              => pr( "Subtraction" ); continue();
                    case Multiplication(l, r)                           => pr( "Multiplication" ); continue();
                    case Division(l, r)                                 => pr( "Division" ); continue();
                    
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
                    
                    case NamedTypeExpr( typeName )                                  => pr( "Named type " + typeName )
                    case ListTypeExpr( expr )                                       => pr( "List type " ); continue();
                    case TypeExpr( elements )                                       => pr( "Type expr" ); continue();
                    case TypeAnnotation( name, typeExpr )                           => pr( "TypeAnnotation " + name ); continue();
                    
                    case VariantClauseDefinition( name, elementTypes )              => pr( "VariantClause " + name + " : " + elementTypes.mkString( " " ) ); continue();
                    case VariantTypeDefinition( clauses )                           => pr( "VariantTypeDefinition" ); continue();
                    case TypeDefinition( typeName, typeParameters, instanceType )   => pr( "TypeDefinition " + typeName + " : " + typeParameters.mkString( " " ) ); continue();
                }
                indent -= 1
            }
        }
        
        TransformAST( expr, new Dumper() )
    }
}
