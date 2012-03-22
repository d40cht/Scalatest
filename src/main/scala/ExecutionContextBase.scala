package org.seacourt.pacatoon

import scala.util.parsing.input.Position
import scala.collection.{mutable, immutable}

class ContextFrame[K, T]
{
    var symbols = new immutable.HashMap[K, T]()
    def set( id : K, value : T )
    {
        symbols += id -> value
    }
    def get( id : K ) = symbols.get(id)
}

class ExecutionContextBase[K, T]( val makeFrame : () => ContextFrame[K, T], val errorMessage : String )
{
    var stack = List( makeFrame() )
    
    def push()
    {
        stack = makeFrame() :: stack
    }
    
    def pop()
    {
        stack = stack.tail
    }
    
    def set( id : K, value : T ) = { stack.head.set( id, value ) }
    
    def getOption( id : K ) =
    {
        def getRec( id : K, stack : List[ContextFrame[K, T]] ) : Option[T] = stack.head.get(id) match
        {
            case Some(value) => Some(value)
            case _ =>
            {
                if (stack.tail == Nil) None
                else getRec( id, stack.tail )
            }
        }
        
        getRec( id, stack )
    }
    
    def get( pos : Position, id : K ) =
    {
        getOption( id ) match
        {
            case Some(value) => value
            case None        => throw new TypeError( pos, errorMessage + ": " + id )
        }
    }
}

