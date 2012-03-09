package org.seacourt.pacatoon

object TestRunner extends Application
{
    override def main( args : Array[String] ) =
    {
        assert( args.length == 1 )
        
        val file = scala.io.Source.fromFile( args(0) )
        val str = file.mkString
        file.close()
        val parsed = CalculatorDSL.parse( str )
        val execContext = new ExecutionContext()
        val evaluator = new DynamicASTEvaluator( execContext )
        evaluator.eval( parsed )
    }
}
