package org.seacourt.pacatoon

import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional


sealed abstract class Expression extends Positional
{
    def exprType = new Untyped()
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

case class IdDefinition( id : String, args : List[String], value : Expression ) extends Expression
case class Apply( lhs : Expression, rhs : Expression ) extends Expression
case class IdExpression( id : String ) extends Expression
case class ExprList( val elements : List[Expression] ) extends Expression
case class BlockScopeExpression( val contents : Expression ) extends Expression
case class IfExpression( val cond : Expression, val trueBranch : Expression, val falseBranch : Expression ) extends Expression


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
    
    lazy val factor: Parser[Expression] = positioned(blockScope | controlFlow | defn | fpLit | stringLit | "(" ~> expr <~ ")" ^^ { e => e } | idExpression ^^ { e => e })
    
    lazy val fpLit : Parser[Expression] = positioned(floatingPointNumber ^^ 
    {
        lit =>
        {
            val isInteger = !lit.foldLeft(false)((x,y) => x || (y=='.'))
            if ( isInteger )
            {
                new Constant( new IntegerValue(lit.toInt) )
            }
            else
            {
                new Constant( new FloatValue(lit.toDouble) )
            }
        } 
    })
    
    lazy val stringLit : Parser[Expression] = positioned(stringLiteral ^^ { str => new Constant( new StringValue( str.drop(1).dropRight(1) ) ) })
 
    lazy val defn : Parser[Expression] = positioned("@def" ~ ident ~ ((ident)*) ~ "=" ~ expr ^^ {
        case "@def" ~ id ~ args ~ "=" ~ e => new IdDefinition( id, args, e )
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


