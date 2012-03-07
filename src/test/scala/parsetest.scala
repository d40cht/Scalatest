import org.scalatest.FunSuite

import scala.util.parsing.combinator._

sealed abstract class Expression
{
    def eval : Double
}

case class Constant( eval : Double ) extends Expression

case class Addition( left : Expression, right : Expression ) extends Expression
{
    def eval = left.eval + right.eval
}

case class Subtraction( left : Expression, right : Expression ) extends Expression
{
    def eval = left.eval - right.eval
}

case class Multiplication( left : Expression, right : Expression ) extends Expression
{
    def eval = left.eval * right.eval
}

case class Division( left : Expression, right : Expression ) extends Expression
{
    def eval = left.eval / right.eval
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
    def factor: Parser[Expression] = fpLit | "(" ~> expr <~ ")" ^^ { e => e }
    def fpLit : Parser[Expression] = floatingPointNumber ^^ { fpLit => new Constant( fpLit.toDouble ) }
    
    def parse( expression : String ) =
    {
        parseAll( expr, expression ) match
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
        val expr = new Division( new Addition( new Constant( 4.0 ), new Constant( 5.0 ) ), new Constant( 3.0 ) )
        assert( expr.eval === 3.0 )
        //assert( (calc "(4.0 + 5.0) / 3.0") === 3.0 )
    }
    
    test("Simple parse test")
    {
        val res1 = CalculatorDSL.parse( "(4.0+5.0)/3.0" )
        assert( res1.eval === 3.0 )
        
        val res2 = CalculatorDSL.parse( "1.0+2.0+3.0" )
        assert( res2.eval === 6.0 )
        
        val res3 = CalculatorDSL.parse( "1.0*2.0*3.0" )
        assert( res3.eval === 6.0 )
        
        val res4 = CalculatorDSL.parse( "2.0+3.0*3.0" )
        assert( res4.eval === 11.0 )
        
        //CalculatorDSL.parse( "(4.0+5.0)/sa3.0" )
    }
}
