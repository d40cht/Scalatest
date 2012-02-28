import org.scalatest.FunSuite

class Test1 extends FunSuite
{
    test("Structural typing")
    {
        def pooker( pookable : { def pook( p : String ) : String } )
        {
            assert( pookable.pook( "pook" ) === "pook" )
        }
        
        class Pookable1
        {
            def pook( p : String ) = p
        }
        
        
        class Pookable2
        {
            def pook( p : String ) = p
        }
        
        pooker( new Pookable1() )
        pooker( new Pookable2() )
    }
    
    test("Pattern match extracting assignments")
    {
        case class T1( val a : String )
        case class T2( val b : T1 )
        
        val q = new T2( new T1( "woot" ) )
        
        val T2( T1( extracted : String ) ) = q
        
        assert( extracted === "woot" )
    }
}

