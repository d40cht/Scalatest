package org.seacourt.pacatoon

object TestRunner extends Application
{
    override def main( args : Array[String] ) =
    {
        assert( args.length == 1 || args.length == 2 )
        
        val file = scala.io.Source.fromFile( args(0) )
        val typeCheck = if ( args.length > 1 && args(1) == "typeCheck" ) true else false
        
        val str = file.mkString
        file.close()
        val parsed = CalculatorDSL.parse( str )
        if (typeCheck)
        {
            buTypeAST( parsed )
            DumpAST( parsed )
        }
        
        val execContext = new ValueExecutionContext()
        val evaluator = new DynamicASTEvaluator( execContext )
        evaluator.eval( parsed )
    }
}
