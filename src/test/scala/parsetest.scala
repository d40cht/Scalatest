import org.scalatest.FunSuite

import scala.util.parsing.combinator._
import scala.collection.{mutable, immutable}

class ExecutionContext
{
    var varValues = new immutable.HashMap[String, Double]()
    def setVar( id : String, value : Double )
    {
        varValues += id -> value
    }
    def getVar( id : String ) = varValues(id)
}

sealed abstract class Expression
{
    def eval( implicit context : ExecutionContext ) : Double
}

case class Constant( value : Double ) extends Expression
{
    def eval( implicit context : ExecutionContext ) = value
}

case class Addition( left : Expression, right : Expression ) extends Expression
{
    def eval( implicit context : ExecutionContext ) = left.eval + right.eval
}

case class Subtraction( left : Expression, right : Expression ) extends Expression
{
    def eval( implicit context : ExecutionContext ) = left.eval - right.eval
}

case class Multiplication( left : Expression, right : Expression ) extends Expression
{
    def eval( implicit context : ExecutionContext ) = left.eval * right.eval
}

case class Division( left : Expression, right : Expression ) extends Expression
{
    def eval( implicit context : ExecutionContext ) = left.eval / right.eval
}

case class VarDefinition( id : String, value : Expression ) extends Expression
{
    def eval( implicit context : ExecutionContext ) =
    {
        val res = value.eval
        context.setVar( id, res )
        res
    }
}

case class Identifier( name : String ) extends Expression
{
    def eval( implicit context : ExecutionContext ) = context.getVar(name)
}

class CalcParseError( msg : String ) extends RuntimeException(msg)

object CalculatorDSL extends JavaTokenParsers
{
    

    def expr: Parser[Expression] = term ~ ((("+"|"-") ~ expr)?) ^^ {
        case e ~ None => e
        case l ~ Some("+" ~ r)    => new Addition( l, r )
        case l ~ Some("-" ~ r)    => new Subtraction( l, r )
    }
    def term: Parser[Expression] = factor ~ ((("*"|"/") ~ term)?) ^^ {
        case e ~ None => e
        case l ~ Some("*" ~ r)   => new Multiplication( l, r )
        case l ~ Some("/" ~ r)    => new Division( l, r )
    }
    def factor: Parser[Expression] = varDefn | identExpr | fpLit | "(" ~> expr <~ ")" ^^ { e => e }
    def fpLit : Parser[Expression] = floatingPointNumber ^^ { fpLit => new Constant( fpLit.toDouble ) }
    
    def identExpr : Parser[Expression] = ident ^^ { x => new Identifier( x ) }
    
    def varDefn : Parser[Expression] = "let" ~ ident ~ "=" ~ expr ^^ {
        case "let" ~ id ~ "=" ~ e => new VarDefinition( id, e )
    }
    
    def exprList : Parser[Expression] =
    {
          expr ~ ((";" ~ exprList)?) ^^ {
            case e ~ None           => e
            case e ~ Some(";" ~ eL) => eL
          }
    }
    
    /*def blockDefn : Parser[Expression] = (
          varDefn ~ ";"
        | expr ~ ";" )*
    */
    
    def parse( expression : String ) =
    {
        parseAll( exprList, expression ) match
        {
            case NoSuccess( msg, next ) => throw new CalcParseError( msg )
            case Success( ast, next ) => ast
        }
    }
}

class CalculatorParseTest extends FunSuite
{
    test("Simple AST test")
    {
        implicit val executionContext = new ExecutionContext()
        
        val expr = new Division( new Addition( new Constant( 4.0 ), new Constant( 5.0 ) ), new Constant( 3.0 ) )
        assert( expr.eval === 3.0 )
    }
    
    test("Simple parse test")
    {
        implicit val executionContext = new ExecutionContext()
        
        val res1 = CalculatorDSL.parse( "(4.0+5.0)/3.0" )
        assert( res1.eval === 3.0 )
        
        val res2 = CalculatorDSL.parse( "1.0+2.0+3.0" )
        assert( res2.eval === 6.0 )
        
        val res3 = CalculatorDSL.parse( "1.0*2.0*3.0" )
        assert( res3.eval === 6.0 )
        
        val res4 = CalculatorDSL.parse( "2.0+3.0*3.0" )
        assert( res4.eval === 11.0 )
        
        val res5 = CalculatorDSL.parse( "2.0*3.0+3.0" )
        assert( res5.eval === 9.0 )
        
        assert( CalculatorDSL.parse( "let x = 12.0" ).eval === 12.0 )
        
        assert( CalculatorDSL.parse( "x * x" ).eval === 144.0 )
        
        assert( CalculatorDSL.parse( "3.0 ; 4.0 ; 5.0" ).eval === 5.0 )
        
        //CalculatorDSL.parse( "(4.0+5.0)/sa3.0" )
    }
}
