package org.seacourt.pacatoon

sealed abstract class ExprType
{
    def skipTypeRef = this
}

case class Untyped extends ExprType
case class TypeUnit extends ExprType
case class TypeFloat extends ExprType
case class TypeBoolean extends ExprType
case class TypeInteger extends ExprType
case class TypeString extends ExprType

case class FunctionType( val argTypes : List[ExprType], val retType : ExprType ) extends ExprType

object GenericType
{
    var lastId = 0
}

case class GenericType( id : Int = GenericType.lastId ) extends ExprType
{
    GenericType.lastId += 1
}

case class TypeReference( val destName : String, var destType : ExprType = new Untyped() ) extends ExprType
{
    override def toString = "TypeReference : " + destName
    override def skipTypeRef = destType.skipTypeRef
}

//case class TupleType( val elementTypes : List[ExprType] ) extends ExprType

// Need to implement typing for lists using variant types. 'ListType'[T]{ 'nil' => TypeUnit, 'cons' => TypeTuple[T]( T, ListType[T] ) }
//
// Start with option type
case class VariantClauseType( val name : String, val elTypes : List[ExprType], val enum : Int ) extends ExprType
case class VariantType( val variants : List[VariantClauseType] ) extends ExprType


trait TypeVisitor
{
    def before( exprType : ExprType ) {}
    def after( exprType : ExprType ) {}
}

object VisitTypes
{
    def apply( exprType : ExprType, visitor : TypeVisitor ) =
    {
        def rec( exprType : ExprType )
        {
            visitor.before(exprType)
            exprType match
            {
                case Untyped()                              =>
                case TypeUnit()                             =>
                case TypeFloat()                            =>
                case TypeBoolean()                          =>
                case TypeInteger()                          =>
                case TypeString()                           =>
                case FunctionType(argTypes, retType)        => argTypes.foreach( x => rec(x) ); rec(retType);
                case GenericType(id)                        =>
                case TypeReference(destName, destType)      => // Do not follow type references
                case VariantClauseType(name, elTypes, enum) => elTypes.foreach( x => rec(x) );
                case VariantType(variants)                  => variants.foreach( x => rec(x) );
            }
            visitor.after(exprType)
        }
    }
}

object DumpTypes
{
    def apply( exprType : ExprType )
    {
        class Dumper extends TypeVisitor
        {
            var indent = 0
            
            override def before( exprType : ExprType )
            {
                def pr( s : String ) = println( ("| "*indent) + s )
                
                exprType match
                {
                    case Untyped()                              => pr( "Untyped" )
                    case TypeUnit()                             => pr( "Unit" )
                    case TypeFloat()                            => pr( "Float" )
                    case TypeBoolean()                          => pr( "Boolean" )
                    case TypeInteger()                          => pr( "Int" )
                    case TypeString()                           => pr( "String" )
                    case FunctionType(argTypes, retType)        => pr( "Function" )
                    case GenericType(id)                        => pr( "Generic" )
                    case TypeReference(destName, destType)      => pr( "TypeReference: " + destName )
                    case VariantClauseType(name, elTypes, enum) => pr( "Clause: " + name )
                    case VariantType(variants)                  => pr( "Variant" )
                
                }
            }
        }
    }
}


