import org.scalatest.FunSuite

import org.seacourt.pacatoon._


class CalculatorParseTest extends FunSuite
{
    def exec[T]( str : String, dump : Boolean = false ) =
    {
        val parsed = CalculatorDSL.parse( str )
        if (dump) DumpAST( parsed )
        val execContext = new ExecutionContext()
        val evaluator = new DynamicASTEvaluator( execContext )
        
        evaluator.eval( parsed ).asInstanceOf[T]
    }
    
    test("Simple parse test")
    {
        assert( exec[FloatValue]( "(4.0+5.0)/3.0" ).value === 3.0 )
        assert( exec[FloatValue]( "1.0+2.0+3.0" ).value === 6.0 )
        assert( exec[FloatValue]( "1.0*2.0*3.0" ).value === 6.0 )
        assert( exec[FloatValue]( "2.0+3.0*3.0").value === 11.0 )
        assert( exec[FloatValue]( "2.0*3.0+3.0").value === 9.0 )
        assert( exec[FloatValue]( "@def x = 12.0; x" ).value === 12.0 )
        assert( exec[FloatValue]( "@def x = 12.0; x * x" ).value === 144.0 )
        assert( exec[FloatValue]( "3.0 ; 4.0 ; 5.0" ).value === 5.0 )
        
        assert( exec[FloatValue](
            "@def y = 10.0;" +
            "@def z = 13.0;" +
            "y * z"
        ).value === 130 )
        
        assert( exec[FloatValue]( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 } } }" ).value === 3.0 )
        assert( exec[FloatValue]( "{ 4.0; { 5.0; { 1.0; 2.0; 3.0 }; 6.0 }; 7.0 }" ).value === 7.0 )
        assert( exec[FloatValue]( "@def y = 10.0; { @def y = 13.0; y }" ).value === 13.0 )
        assert( exec[FloatValue]( "@def y = 10.0; @def z = y; z" ).value === 10.0 )
        
        assert( exec[BooleanValue]( "4.0 < 5.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 <= 5.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 > 5.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 >= 5.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 == 5.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 != 5.0" ).value === true )
        
        assert( exec[BooleanValue]( "4.0 < 4.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 <= 4.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 > 4.0" ).value === false )
        assert( exec[BooleanValue]( "4.0 >= 4.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 == 4.0" ).value === true )
        assert( exec[BooleanValue]( "4.0 != 4.0" ).value === false )
        
        assert( exec[IntegerValue]( "4 * 5" ).value === 20 )
        
        
        //assert( exec[FloatValue]( "@def x : float = 4.0; x" ).value === 4.0 )
        //assert( exec[IntegerValue]( "@def x : int = 4; x" ).value === 4 )
    }
    
    val mapFn = 
        "@def map fn l =" +
        "{" +
        "    @if (l == nil) nil" +
        "    @else (fn (head l)) :: (map fn (tail l))" +
        "};"

    
    test("If expression")
    {
        assert( exec[FloatValue](
            "@def x = 12.0;" +
            "@def y = 13.0;" +
            "@def ret = 0.0;" +
            "@if ( x < y ) { 4.0 } @else { 5.0 }", dump=true
        ).value === 4.0 )    
    }
    
    test("If expression with chained elses")
    {
        assert( exec[FloatValue](
            "@if ( 3.0 < 4.0 ) 5.0 @else @if ( 4.0 < 5.0 ) 6.0 @else @if ( 5.0 < 6.0 ) 7.0 @else 8.0"
        ).value === 5.0 )    
    }
    
    test("Simple function calls")
    {
        assert( exec[FloatValue]( 
            "@def double x = x * 2.0;" +
            "double 5.0"
        ).value === 10.0 )
        
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "sum 3.0 5.0"
        ).value === 8.0 )
    }
    
    test( "Built in string conversion" )
    {
        assert( exec[StringValue]( "\"Here \" + (toString 3.0) + \" \" + (toString 5)" ).value === "Here 3.0 5" )
    }
    
    test("Function calls with arithmetic expressions as arguments")
    {
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "sum 2.0+1.0 2.0+3.0"
        ).value === 8.0 )
    }
     
    test("Partial application syntax(sort of)")
    {
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "(sum 3.0) 5.0"
        ).value === 8.0 )
    }
     
    test("Simple function calls with variables as args")
    {
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "@def p = 3.0;" +
            "@def q = 5.0;" +
            "sum p q"
        ).value === 8.0 )
    }

    test("Simple recursion")
    {   
        assert( exec[FloatValue]( 
            "@def sumSeries x = @if (x==0.0) 0.0 @else x+(sumSeries (x + (-1.0)));" +
            "sumSeries 5.0"
        ).value === 15.0 )
    }
    
    test("Function as a first class object" )
    {
        assert( exec[FloatValue]( 
            "@def sum x y = x + y;" +
            "@def sumcp = sum;" +
            "sumcp 3.0 5.0"
        ).value === 8.0 )     
    }
    
    test("Partial function application" )
    {
        assert( exec[IntegerValue](
            "@def sum x y = x + y;" +
            "@def inc = sum 1;" +
            "inc 4" ).value === 5 )
    }
    
    test("Function as function parameter" )
    {
        assert( exec[FloatValue](
            "@def sum x y = x + y;" +
            "@def mul x y = x * y;" +
            "@def apply fn x y = fn x y;" +
            "(apply sum 2.0 3.0) + (apply mul 4.0 5.0)"
        ).value === 25.0 )
    }
    
    test("Lists 1")
    {
        assert( exec[FloatValue](
            "@def l = 1.0 :: 2.0 :: 3.0 :: nil;" +
            "head l"
        ).value === 1.0 )
    }
    
    test("Lists 2")
    {
        assert( exec[FloatValue](
            "@def l = 1.0 :: 2.0 :: 3.0 :: nil;" +
            "head (tail (tail l))"
        ).value === 3.0 )
    }
    
    test("Lists 3")
    {
        assert( exec[BooleanValue](
            "@def l = 1.0 :: 2.0 :: 3.0 :: nil;" +
            "@def last = tail (tail (tail l));" +
            "last == nil"
        ).value === true )
    }
    
    
    test( "Closures" )
    {
        assert( exec[FloatValue](
            "@def incrementer count = (@def _ x = x+count);" +
            "@def addTwo = incrementer 2.0;" +
            "@def addFive = incrementer 5.0;" +
            "(addTwo 7.0) + (addFive 2.0)"
        ).value === 16.0 )
    }
    
    /*test( "Closures2: Manual (out of order) partial application" )
    {
        assert( exec[FloatValue](
            "@def divide x y = x / y;" +
            "@def fixedDivide x = (@def _ y = divide y / x);" +
            "@def halve = fixedDivide 2.0;" +
            "@def third = fixedDivide 3.0;" +
            "(halve 16.0) + (third 9.0)"
        ).value === 11.0 )
    }*/
    
    test("Blocks")
    {
        assert( exec[FloatValue]( "{ 1.0; 2.0; 3.0; 4.0 }" ).value === 4.0 )
    }
    
    test("Functional tools: map")
    {
        assert( exec[ListElementValue](
            mapFn +
            "map (@def square x=x*x) (1.0::2.0::3.0::4.0::nil)"
        ).toString === "(1.0::4.0::9.0::16.0::nil)" )
    }
    
    test("Functional tools: foldLeft")
    {
        assert( exec[FloatValue](
            "@def foldLeft fn acc l =" +
            "{" +
            "    @if (l == nil) acc" +
            "    @else (fn (head l) (foldLeft fn acc (tail l)))" +
            "};" +
            "foldLeft (@def sum x y=x+y) 0.0 (1.0::2.0::3.0::4.0::nil)"
        ).value === 10.0 )
    }
    
    test( "Mergesort" )
    {
        // Wouldn't it be nice to have pattern matching?
        val mergeFn =
            "@def merge l1 l2 =" +
            "{" +
            "    @if ((l1 == nil) && (l2 == nil)) nil" +
            "    @else @if (l1 == nil) ((head l2) :: (merge l1 (tail l2)))" +
            "    @else @if (l2 == nil) ((head l1) :: (merge (tail l1) l2))" +
            "    @else @if ((head l1) < (head l2)) ((head l1) :: (merge (tail l1) l2))" +
            "    @else ((head l2) :: (merge l1 (tail l2)))" +
            "};"

        assert( exec[ListElementValue]( mergeFn + "merge (1.0 :: 3.0 :: 5.0 :: nil) (0.0 :: 2.0 :: 4.0 :: 6.0 :: nil)" ).toString === "(0.0::1.0::2.0::3.0::4.0::5.0::6.0::nil)" )
        assert( exec[ListElementValue]( mergeFn + "merge (merge (merge (3.0 :: nil) (4.0 :: nil)) (merge (1.0 :: nil) (2.0::nil))) (merge (0.0 :: nil) (5.0::nil))" ).toString === "(0.0::1.0::2.0::3.0::4.0::5.0::nil)" )
        
        assert( exec[ListElementValue](
            mapFn + 
            mergeFn +
            "@def unordered = 5.0::1.0::3.0::2.0::4.0::0.0::6.0::nil;" +
            "@def elsAsLists = map (@def _ x = x::nil) unordered;" +
            "@def mergePairs l =" +
            "{" +
            "    @if (l==nil) nil" +
            "    @else" +
            "    {" +
            "        @def first = head l;" +
            "        @def tail1 = tail l;" +
            "        @if ( tail1 == nil ) first :: nil" +
            "        @else (merge first (head tail1)) :: (mergePairs (tail tail1))" +
            "    }" +
            "};" +
            "@def mergePairsRec l = @if ((tail l) == nil) (head l) @else (mergePairsRec (mergePairs l));" +
            "mergePairsRec elsAsLists" ).toString === "(0.0::1.0::2.0::3.0::4.0::5.0::6.0::nil)" )
            
            
        
        /*val splitFn =
            "@def split l l1 l2=" +
            "{" +
            "    @if ( l == nil ) l1 :: l2 :: nil
            "    @if ( (l != nil) && ((tail l) != nil) ) split (tail (tail l)) (head l) (head (tail l))" +
            "    @else @if ( */
    }
    
    
    /*<<mergesort>>=
        let rec mergesort compare = 
	        split
          in
	        merge
	        in function
          | ([] | [_]) as l -> l
          | l ->  let left,right = split l in 
                  merge compare (mergesort compare left) (mergesort compare right)
        ;;
    */
    
    /*test("Record type")
    {
        exec[FloatValue]( "@type Test = { a : double, b : double }" )
        assert( exec[FloatValue](
            "@type Test = { a : Double, b : Double };" +
            "@def t = Test( a = 12.0, b = 15.0 );" +
            "t.a + t.b"
        ).value === 23.0 )
    }*/
}
