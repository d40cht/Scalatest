import org.scalatest.FunSuite

import scala.collection.mutable.{ListBuffer}

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
    
    test( "Closures and their references" )
    {
        var a = 12
        var b = 12
        
        def t1 = a + b
        
        assert( t1 === 24 )
        
        // Closures take references to the external objects, not copies thereof
        a = 13
        assert( t1 === 25 )
    }
    
    test( "Curried functions" )
    {
        def blah( a : Int )( b : Int ) = a + b
        
        assert( blah(1)(2) === 3 )
        
        assert( (blah {4} {5} ) === 9 )
    }
    
    test( "Code blocks as function arguments" )
    {
        def runner( fn : => Unit ) = fn
        def notrunner( fn : => Unit ) = Unit
        
        var a = 0
        
        def inc() = { a += 1 }
        
        assert( a === 0 )
        inc()
        assert( a === 1 )
        runner( inc() )
        assert( a === 2 )
        
        // This should not run the function body
        notrunner( inc() )
        assert( a === 2 )
    }
    
    test( "Partial functions" )
    {
        val a = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
        
        val b = (a collect { case v if v >= 12 => v * v }).sum
        
        assert( b === (12*12) + (13*13) + (14*14) )
        

        // Passing a partial function to map will fail at runtime if not defined on the input
        intercept[scala.MatchError]( a map { case v if v >= 12 => v * v } )
    }
    
    test( "Maps etc" )
    {
        val b = Map(
            1 -> '1',
            2 -> '2',
            3 -> '3' )
        val c = List( (1, '1'), (2, '2'), (3, '3') )
        
        assert( b === c.toMap )
        
        val v = List( 1, 2, 3, 1, 2, 3, 1, 2, 3 )
        assert( v.toSet.size === 3 )
        
        assert( v.grouped(3).toList startsWith List( List( 1, 2, 3 ), List( 1, 2, 3 ) ) )
        assert( v.sliding(3).toList startsWith List( List( 1, 2, 3 ), List( 2, 3, 1 ), List( 3, 1, 2 ) ) )
        
        val q = List( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 )
        assert( q containsSlice List( 4, 5, 6 ) )
        
        
        assert( v.distinct.toList sameElements List( 1, 2, 3 ) )
    }
    
    test( "Buffers" )
    {
        // TODO
    }
    
    test( "Sets" )
    {
        val a = scala.collection.immutable.SortedSet( 1, 2, 3, 4, 5, 6 )
        val b = scala.collection.immutable.SortedSet( 4, 5, 6, 7, 8, 9, 10 )
        
        assert( Set( 3, 4 ) subsetOf a )
        assert( !(a subsetOf b) )
        
        assert( (a intersect b) sameElements List (4, 5, 6) )
        
        // Note diff is not symmetric
        assert( (a diff b) sameElements List( 1, 2, 3 ) )
        assert( (b diff a) sameElements List( 7, 8, 9, 10 ) )
        
        def symDiff[T]( a : scala.collection.immutable.SortedSet[T], b : scala.collection.immutable.SortedSet[T] ) = (a diff b) union (b diff a)
        
        assert( symDiff(a, b) sameElements List( 1, 2, 3, 7, 8, 9, 10 ) )
        
        // As above, but with implicit conversion magic
        {
            class SymdiffableSet[T]( val contents : scala.collection.immutable.SortedSet[T] )
            {
                def symDiff( other : SymdiffableSet[T] ) = new SymdiffableSet( (contents diff other.contents) union (other.contents diff contents) )
            }
            
            object convs
            {
                implicit def setToSymdiffableSet[T]( set : scala.collection.immutable.SortedSet[T] ) = new SymdiffableSet( set )
                implicit def symdiffableSetToSet[T]( sdset : SymdiffableSet[T] ) = sdset.contents
            }
            
            import convs._
            
            assert( (a symDiff b) sameElements List( 1, 2, 3, 7, 8, 9, 10 ) )
        }
        
        assert( !((a - 4) contains 4) )
        assert( (a + 12) contains 12 )
        
        val q = scala.collection.BitSet( 1, 5, 6 )
        assert( q.contains( 5 ) )
    }
    
    test( "Varargs" )
    {   
        def makeList[T]( args : T* ) =
        {
            val l = new ListBuffer[T]()
            for (arg <- args )
            {
                l.append( arg )
            }
            
            l.toList
        }
        
        assert( makeList( 1, 2, 3, 4, 5 ) sameElements List( 1, 2, 3, 4, 5 ) )
    }
    
    test( "Infinite streams with lazy construction" )
    {
    
        def factorials() = 
        {
            // Not stream concatenation operator, akin to :: but with lazy eval
            def rec( curr : Int, index : Int ) : Stream[Int] = curr #:: rec( curr*index, index + 1 )
            
            rec( 1, 1 )
        }
        
        assert( factorials.take(10) sameElements List(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880) )
    }
    
    test( "Queues" )
    {
        var q = scala.collection.immutable.Queue[Int]()
        
        q = q.enqueue( 1 )
        q = q.enqueue( 2 )
        q = q.enqueue( 3 )
        
        assert( q.length === 3 )
        
        q += 4
        assert( q.length == 4 )
        
        val( element1, newq1 ) = q.dequeue
        assert( element1 === 1 )
        
        val( element2, newq2 ) = newq1.dequeue
        assert( element2 === 2 )
        
        val( element3, newq3 ) = newq2.dequeue
        assert( element3 === 3 )
        
        assert( newq3.length === 1 )
    }
    
    test( "Views" )
    {
        val nums = 0 until 1000
        val operateOn = nums.view.map( _ + 3 ).map( x => x * x ).force
        assert( operateOn.take( 4 ) sameElements List( 9, 16, 25, 36 ) )
        
        val els2 = Array( 4, 5, 6, 7 )
        val v = els2.view.slice( 1, 3 )
        println( v.toList )
        assert( v sameElements List( 5, 6 ) )
        
        els2(1) = 12
        
        println( v.toList )
        assert( v sameElements List( 12, 6 ) ) 
    }
    
    test( "Implicit parameters" )
    {
        class SomeState( val name : String )
        
        def addPrefix( word : String )( implicit ss : SomeState ) = ss.name + word
        
        {
            implicit val v = new SomeState( "voo" )
            assert( addPrefix( "Bling" ) === "vooBling" )
        }
        
        {
            implicit val v = new SomeState( "opo" )
            assert( addPrefix( "Blong" ) === "opoBlong" )
        }
    }
    
    test( "Manifests" )
    {
    }
    
    test( "Collection munging" )
    {
        val a = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
        
        val slice1 = a.takeWhile( _ < 13 ).dropWhile( _ > 5 )
        
        val b = a.view.map( x => x*x ).map( y => y + y )
    }
    
    
    test( "Blocks as arguments" )
    {
        import resource._
        
        for ( file1 <- managed( new java.io.FileOutputStream( "boo1.txt" ) );
              file2 <- managed( new java.io.FileOutputStream( "boo1.txt" ) ) )
        {
            //file1.write( "hello".toByteArray )
            //file2.write( "world".toByteArray )
        }
        
        /*trait Cleanupable
        {
            def cleanup() : Unit
        }
        
        class Wrapper( var value : String )
        
        class Tester( val fileName : String ) extends Cleanupable
        {
            val file = new java.io.File(fileName)
            
            def cleanup() = { file.close() }
        }
        
        def withCleanup[T <: Cleanupable]( cp : T )( block : T => Unit )
        {
            block( cp )
            cp.cleanup()
        }
        
        withCleanup( new FileCloseable( "test.txt" ) )
        {
            theFile =>
            
            print( theFile )
        }*/
    }
}

