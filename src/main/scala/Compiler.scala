package org.seacourt.pacatoon

class Compiler
{
    def run( str : String )
    {
        val parsed = CalculatorDSL.parse( str )
        val ssaResolved = NameAliasResolution( parsed )
        
        buTypeAST( ssaResolved )
        bytecode.ByteCodeGenerator( parsed )
        val execContext = new ValueExecutionContext()
        val evaluator = new DynamicASTEvaluator( execContext )
        evaluator.eval( ssaResolved )
    }
}
