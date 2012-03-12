package org.seacourt.pacatoon

import scala.collection.{mutable, immutable}
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

sealed abstract class ExprType

case class Untyped extends ExprType
case class TypeUnit extends ExprType
case class TypeFloat extends ExprType
case class TypeBoolean extends ExprType
case class TypeInteger extends ExprType

case class FunctionType( val argTypes : List[ExprType], val retType: ExprType ) extends ExprType

case class TupleType( val elementTypes : List[ExprType] ) extends ExprType

// Need to implement typing for lists using variant types. 'ListType'[T]{ 'nil' => TypeUnit, 'cons' => TypeTuple[T]( T, ListType[T] ) }
//
// Start with option type
case class VariantType( val variants : immutable.HashMap[String, ExprType] ) extends ExprType




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
                def pr( s : String ) = println( ("| "*indent) + s + " : " + expr.exprType.toString )
                
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
                    
                    case IdDefinition( id, params, value : Expression ) => pr( "IdDefinition " + id )
                    case Apply( l, r )                                  => pr( "Apply" )
                    case IdExpression( id )                             => pr( "Id: " + id )
                    case ExprList( elements )                           => pr( "ExprList" )
                    case BlockScopeExpression( contents )               => pr( "BlockScope" )
                    case IfExpression( cond, trueBranch, falseBranch )  => pr("IfExpression")
                    case TypeAnnotation( name, typeNames )              => pr( "TypeAnnotation " + name + " : " + typeNames.mkString( " -> " ) )
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

class ParserError( msg : String ) extends RuntimeException(msg)

object CalculatorDSL extends RegexParsers with PackratParsers
{
    // TODO: Sort keyword parsing out to avoid having the minging '@' prefix
    def ident: Parser[String] = """[a-zA-Z_]\w*""".r
    def wholeNumber: Parser[String] = """-?\d+""".r
    def decimalNumber: Parser[String] = """(\d+(\.\d*)?|\d*\.\d+)""".r
    def stringLiteral: Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
    def floatingPointNumber: Parser[String] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    
    def comment: Parser[Expression] = """//[^\r\n]*""".r ^^ { x => new NullExpression() }
    
    
    def buildApply( initial : Expression, terms : List[Expression] ) =
    {
        terms.foldLeft( initial )( (lhs, rhs) => new Apply( lhs, rhs ) )
    }
    
    lazy val expr : Parser[Expression] = positioned(term4 ~ ((term4)*) ^^ {
        case x ~ Nil    =>  x
        case x ~ y      => buildApply(x, y) })
        
    lazy val term4: Parser[Expression] = positioned(term3 ~ ((("&&"|"||") ~ expr)?) ^^
    {
        case e ~ None => e
        case l ~ Some("&&" ~ r)     => new LogicalAnd( l, r )
        case l ~ Some("||" ~ r)     => new LogicalOr( l, r )
    })
    
    lazy val term3: Parser[Expression] = positioned(term2 ~ ((("<="|">="|"=="|"!="|"<"|">") ~ expr)?) ^^
    {
        case e ~ None => e
        case l ~ Some("<=" ~ r)     => new CmpLe( l, r )
        case l ~ Some(">=" ~ r)     => new CmpGe( l, r )
        case l ~ Some("==" ~ r)     => new CmpEq( l, r )
        case l ~ Some("!=" ~ r)     => new CmpNe( l, r )
        case l ~ Some("<" ~ r)      => new CmpLt( l, r )
        case l ~ Some(">" ~ r)      => new CmpGt( l, r )
    })
    
    lazy val term2: Parser[Expression] = positioned(term1 ~ ((("+"|"-") ~ term2)?) ^^ {
        case e ~ None => e
        case l ~ Some("+" ~ r)      => new Addition( l, r )
        case l ~ Some("-" ~ r)      => new Subtraction( l, r )
    })
    
    lazy val term1: Parser[Expression] = positioned(term0 ~ ((("*"|"/") ~ term1)?) ^^ {
        case e ~ None => e
        case l ~ Some("*" ~ r)      => new Multiplication( l, r )
        case l ~ Some("/" ~ r)      => new Division( l, r )
    })
    
    lazy val term0: Parser[Expression] = positioned(factor ~ (("::" ~ term1)?) ^^ {
        case e ~ None => e
        case l ~ Some("::" ~ r)     => new ListAppend( l, r )
    })
    
    lazy val idExpression : Parser[Expression] = positioned(ident ^^ { x => new IdExpression(x) })
    
    lazy val factor: Parser[Expression] = positioned(blockScope | controlFlow | typeAnnotation | defn | fpLit | stringLit | "(" ~> expr <~ ")" ^^ { e => e } | idExpression ^^ { e => e })
    
    lazy val fpLit : Parser[Expression] = positioned(floatingPointNumber ^^ 
    {
        lit =>
        {
            val isInteger = !lit.foldLeft(false)((x,y) => x || (y=='.'))
            if ( isInteger )
            {
                val c = new Constant( new IntegerValue(lit.toInt) )
                c.exprType = new TypeInteger()
                c
            }
            else
            {
                val c = new Constant( new FloatValue(lit.toDouble) )
                c.exprType = new TypeFloat()
                c
            }
        } 
    })
    
    lazy val stringLit : Parser[Expression] = positioned(stringLiteral ^^ { str => new Constant( new StringValue( str.drop(1).dropRight(1) ) ) })
 
    lazy val defn : Parser[Expression] = positioned("@def" ~ ident ~ ((ident)*) ~ "=" ~ expr ^^ {
        case "@def" ~ id ~ params ~ "=" ~ e => new IdDefinition( id, params, e )
    })
    
    lazy val typeAnnotation : Parser[Expression] = positioned("@def" ~ ident ~ "::" ~ ident ~ (("->" ~> ident)*) ^^ {
        case "@def" ~ id ~ "::" ~ type1 ~ remainingTypes => new TypeAnnotation( id, type1 :: remainingTypes )
    })
    
    lazy val topLevel = positioned(comment | expr ^^ { x => x })
    
    lazy val exprList : Parser[ExprList] = positioned(topLevel ~ ((((";")?) ~ exprList)?) ^^ {
        case e ~ None           => new ExprList( e :: Nil )
        case e ~ Some(_ ~ eL)   => new ExprList( e :: eL.elements )
    })
    
    lazy val controlFlow : Parser[Expression] = positioned("@if" ~ "(" ~ expr ~ ")" ~ expr ~ (("@else" ~ expr)?) ^^
    {
        case "@if" ~ "(" ~ cond ~ ")" ~ trueBranch ~ None                           => new IfExpression( cond, trueBranch, new NullExpression() )
        case "@if" ~ "(" ~ cond ~ ")" ~ trueBranch ~ Some("@else" ~ falseBranch)    => new IfExpression( cond, trueBranch, falseBranch )
    })
    
    lazy val blockScope : Parser[Expression] = positioned("{" ~> exprList <~ "}" ^^ { e => new BlockScopeExpression( e ) })

    def parse( expression : String ) =
    {
        parseAll( exprList, expression ) match
        {
            case NoSuccess( msg, next ) => throw new ParserError( "(line " + next.pos.line + ", column " + next.pos.column + "): " + msg )
            case Success( ast, next ) => ast
        }
    }
}


