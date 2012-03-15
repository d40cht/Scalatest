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
case class ListAppend( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Addition( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Subtraction( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Multiplication( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )
case class Division( _left : Expression, _right : Expression ) extends BinOpExpression( _left, _right )

case class IdDefinition( id : String, params : List[String], value : Expression ) extends Expression
case class Apply( lhs : Expression, rhs : Expression ) extends Expression
case class IdExpression( id : String ) extends Expression
case class ExprList( val elements : List[Expression] ) extends Expression
case class BlockScopeExpression( val contents : Expression ) extends Expression
case class IfExpression( val cond : Expression, val trueBranch : Expression, val falseBranch : Expression ) extends Expression

case class TypeAnnotation( val name : String, val typeNames : List[String] ) extends Expression

case class VariantClauseDefinition( clauseName : String, elementTypeNames : List[String] ) extends Expression
case class VariantTypeDefinition( clauses : List[VariantClauseDefinition] ) extends Expression
case class TypeDefinition( typeName : String, typeParameters : List[String], instanceType : VariantTypeDefinition ) extends Expression

trait ASTVisitor
{
    def before( expr : Expression ) {}
    def after( expr : Expression ) {}
}

object VisitAST
{
    def apply( expr : Expression, visitor : ASTVisitor ) =
    {
        def rec( expr : Expression )
        {
            visitor.before(expr)
            expr match
            {
                case NullExpression()                               => 
                case Constant(v)                                    =>
                
                case LogicalAnd(l, r)                               => rec(l);rec(r);
                case LogicalOr(l, r)                                => rec(l);rec(r);
                case CmpLt(l, r)                                    => rec(l);rec(r);
                case CmpLe(l, r)                                    => rec(l);rec(r);
                case CmpGt(l, r)                                    => rec(l);rec(r);
                case CmpGe(l, r)                                    => rec(l);rec(r);
                case CmpEq(l, r)                                    => rec(l);rec(r);
                case CmpNe(l, r)                                    => rec(l);rec(r);
                
                case ListAppend(l, r)                               => rec(l);rec(r);
                case Addition(l, r)                                 => rec(l);rec(r);
                case Subtraction(l, r)                              => rec(l);rec(r);
                case Multiplication(l, r)                           => rec(l);rec(r);
                case Division(l, r)                                 => rec(l);rec(r);
                
                
                case IdDefinition( id, params, value : Expression ) => rec(value);
                case Apply( l, r )                                  => rec(l); rec(r);
                case IdExpression( id )                             => 
                case ExprList( elements )                           => elements.foreach( e => rec(e) );
                case BlockScopeExpression( contents )               => rec( contents )
                case IfExpression( cond, trueBranch, falseBranch )  => rec(cond); rec(trueBranch); rec(falseBranch);
                case TypeAnnotation( name, typeNames )              =>
                
                case VariantClauseDefinition( name, elementTypes )              =>
                case VariantTypeDefinition( clauses )                           => clauses.foreach( c => rec(c) );
                case TypeDefinition( typeName, typeParameters, instanceType )   => rec(instanceType);
            }
            visitor.after(expr)
        }
        
        rec( expr )
    }
}


object DumpAST
{
    def apply( expr : Expression )
    {
        class Dumper extends ASTVisitor
        {
            var indent = 0
            
            override def before( expr : Expression )
            {
                def pr( s : String ) = println( ("| "*indent) + s )// + " : " + expr.exprType.toString )
                
                expr match
                {
                    case NullExpression()                               => pr( "Null" )
                    case Constant(v)                                    => pr( "Constant: " + v.toString )
                    case LogicalAnd(l, r)                               => pr( "LogicalAnd" )
                    case LogicalOr(l, r)                                => pr( "Division" )
                    case CmpLt(l, r)                                    => pr( "CmpLt" )
                    case CmpLe(l, r)                                    => pr( "CmpLe" )
                    case CmpGt(l, r)                                    => pr( "CmpGt" )
                    case CmpGe(l, r)                                    => pr( "CmpGe" )
                    case CmpEq(l, r)                                    => pr( "CmpEq" )
                    case CmpNe(l, r)                                    => pr( "CmpNe" )
                    
                    case ListAppend(l, r)                               => pr( "ListAppend" )
                    case Addition(l, r)                                 => pr( "Addition" )
                    case Subtraction(l, r)                              => pr( "Subtraction" )
                    case Multiplication(l, r)                           => pr( "Multiplication" )
                    case Division(l, r)                                 => pr( "Division" )
                    
                    case IdDefinition( id, params, value : Expression ) =>
                    {
                        pr( "IdDefinition " + id )
                        DumpTypes( value.exprType )
                    }
                    case Apply( l, r )                                  => pr( "Apply" )
                    case IdExpression( id )                             => pr( "Id: " + id )
                    case ExprList( elements )                           => pr( "ExprList" )
                    case BlockScopeExpression( contents )               => pr( "BlockScope" )
                    case IfExpression( cond, trueBranch, falseBranch )  => pr( "IfExpression" )
                    
                    case TypeAnnotation( name, typeNames )                          => pr( "TypeAnnotation " + name + " : " + typeNames.mkString( " -> " ) )
                    case VariantClauseDefinition( name, elementTypes )              => pr( "VariantClause " + name + " : " + elementTypes.mkString( " " ) )
                    case VariantTypeDefinition( clauses )                           => pr( "VariantTypeDefinition" )
                    case TypeDefinition( typeName, typeParameters, instanceType )   => pr( "TypeDefinition " + typeName + " : " + typeParameters.mkString( " " ) );
                }
                indent += 1
            }
            override def after( expr : Expression )
            {
                indent -= 1
            }
        }
        
        VisitAST( expr, new Dumper() )
    }
}

