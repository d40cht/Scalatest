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
    
    test("Function application infix")
    {
        assert( (List(4, 5, 6) map { _ * 2 }) === List(8, 10, 12) )
    }
    
    test( "Parallel container stuff" )
    {
        val a = (0 until 10000).par.reduce( _ + _ )
        assert( a === 49995000 )
    }
    
    test( "Named arguments" )
    {
        object Foo
        {
            def a( first : Int, second : Int ) = first / second
        }
        
        
        assert( Foo.a( first=12, second=2 ) === 6 )
        assert( Foo.a( second=2, first=12 ) === 6 )
    }
    
    test( "Cake pattern" )
    {
        trait Foo1
        {
            self : Foo1 with Foo2 with Foo3 =>
            
            def one = 1
            def sum1 = one + two + three
        }
        
        trait Foo2
        {
            self : Foo2 with Foo1 with Foo3 =>
            
            def two = 2
            def sum2 = one + two + three
        }
        
        trait Foo3
        {
            self : Foo3 with Foo1 with Foo2 =>
            
            def three = 3
            def sum3 = one + two + three
        }
        
        object Cake extends Foo1 with Foo2 with Foo3
        {
            assert( sum1 === 6 )
            assert( sum2 === 6 )
            assert( sum3 === 6 )
        }
    }
}

