package org.seacourt.pacatoon

import scala.util.parsing.input.Position
import scala.collection.{mutable, immutable}

class ContextFrame[T]
{
    var symbols = new immutable.HashMap[String, T]()
    def set( id : String, value : T )
    {
        symbols += id -> value
    }
    def get( id : String ) = symbols.get(id)
}

class ExecutionContextBase[T]( val makeFrame : () => ContextFrame[T], val errorMessage : String )
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
    
    def set( id : String, value : T ) = { stack.head.set( id, value ) }
    
    def getOption( id : String ) =
    {
        def getRec( id : String, stack : List[ContextFrame[T]] ) : Option[T] = stack.head.get(id) match
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
    
    def get( pos : Position, id : String ) =
    {
        getOption( id ) match
        {
            case Some(value) => value
            case None        => throw new TypeError( pos, errorMessage + ": " + id )
        }
    }
}

