import org.scalatest.FunSuite

//import scala.util.parsing.combinator.

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

class CalculatorParseTest extends FunSuite
{
    test("Simple AST test")
    {
        val expr = new Division( new Addition( new Constant( 4.0 ), new Constant( 5.0 ) ), new Constant( 3.0 ) )
        assert( expr.eval === 3.0 )
        //assert( (calc "(4.0 + 5.0) / 3.0") === 3.0 )
    }
}
