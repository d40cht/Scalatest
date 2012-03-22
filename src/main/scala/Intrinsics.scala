package org.seacourt.pacatoon

import scala.collection.{mutable, immutable}

class Intrinsic( val name : String, val iType : ExprType, val value : BaseValue )
{
    val id = new Identifier( name )
    id.setType( iType )
}

object Intrinsics
{
    var members = new mutable.ArrayBuffer[Intrinsic]()
    
    members.append( new Intrinsic( "nil", new TypeList( new TypeGeneric() ), new ListTerminatorValue() ) )
    
    members.append( new Intrinsic( "toString", new TypeFunction( List( new TypeGeneric() ), TypeString ),
        new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "toString function takes only one parameter" )
            new StringValue( args(0).toString )
        } ) ) )
        
    members.append( new Intrinsic( "print", new TypeFunction( List( TypeString ), TypeUnit ),
        new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "print function takes only one parameter" )
            println( args(0) );
            new UnitValue();
        } ) ) )
    
    val elType = new TypeGeneric()
    members.append( new Intrinsic( "head", new TypeFunction( List( new TypeList(elType) ), elType ),
        new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "head function takes only one parameter" )
            
            args(0) match
            {
                case ListElementValue( head, tail ) => head
                case ListTerminatorValue() => throw new TypeError( pos, "Calling head on empty list" )
                case _ => throw new TypeError( pos, "Calling head on non-list type" )
            }
        } ) ) )

    val listType = new TypeList( new TypeGeneric() )
    members.append( new Intrinsic( "tail", new TypeFunction( List( listType ), listType ),
        new BuiltInFunction( 1, (pos, args) =>
        {
            if ( args.length != 1 ) throw new TypeError( pos, "tail function takes only one parameter" )
            
            args(0) match
            {
                case ListElementValue( head, tail ) => tail
                case ListTerminatorValue() => throw new TypeError( pos, "Calling head on empty list" )
                case _ => throw new TypeError( pos, "Calling head on non-list type" )
            }
        } ) ) )

    val comparisonType = new TypeGeneric()
    members.append( new Intrinsic( "assertEqual", new TypeFunction( List( comparisonType, comparisonType ), TypeUnit ),
        new BuiltInFunction( 2, (pos, args) =>
        {
            if ( args.length != 2 ) throw new TypeError( pos, "assertEqual function takes two parameters" )
            val (a, b) = (args(0), args(1))
            if ( a != b )
            {
                throw new AssertionFailure( pos, a.toString + " != " + b.toString )
            }
            else
            {
                println( "[Assertion passed]" )
            }
            new UnitValue();
        } ) ) )
}
