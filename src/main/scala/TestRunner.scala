package org.seacourt.pacatoon

object TestRunner extends Application
{
    override def main( args : Array[String] ) =
    {
        val typeCheck = true
        
        for ( f <- args )
        {
            println( "Evaluation: " + f )
            val file = scala.io.Source.fromFile( f )
            val str = file.mkString
            file.close()
            val parsed = CalculatorDSL.parse( str )
            if (typeCheck)
            {
                //DumpAST( parsed )
                buTypeAST( parsed )
                //DumpAST( parsed )
            }
            
            val execContext = new ValueExecutionContext()
            val evaluator = new DynamicASTEvaluator( execContext )
            evaluator.eval( parsed )
        }
    }
}
