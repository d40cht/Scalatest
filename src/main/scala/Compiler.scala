package org.seacourt.pacatoon

class Compiler
{
    def run( str : String )
    {
        val parsed = CalculatorDSL.parse( str )
        val ssaResolved = NameAliasResolution( parsed )
        buTypeAST( ssaResolved )
        val noClosures = LiftAllFunctions( ssaResolved )
        bytecode.ByteCodeGenerator( noClosures )
        
        val execContext = new ValueExecutionContext()
        val evaluator = new DynamicASTEvaluator( execContext )
        evaluator.eval( noClosures )
    }
}
