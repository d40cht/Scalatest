package org.seacourt.pacatoon

sealed abstract class ExprType
{
    def skipTypeRef = this
}

case object TypeNone extends ExprType
case object TypeUnit extends ExprType
case object TypeFloat extends ExprType
case object TypeBoolean extends ExprType
case object TypeInteger extends ExprType
case object TypeString extends ExprType

case class TypeList( val elType : ExprType ) extends ExprType

case class TypeFunction( val argTypes : List[ExprType], val retType : ExprType ) extends ExprType

object TypeGeneric
{
    var lastId = 0
}

case class TypeGeneric( id : Int = TypeGeneric.lastId ) extends ExprType
{
    TypeGeneric.lastId += 1
}

case class TypeReference( val destName : String, var destType : ExprType = TypeNone ) extends ExprType
{
    override def toString = "TypeReference : " + destName
    override def skipTypeRef = destType.skipTypeRef
}

//case class TupleType( val elementTypes : List[ExprType] ) extends ExprType

// Need to implement typing for lists using variant types. 'TypeList'[T]{ 'nil' => TypeUnit, 'cons' => TypeTuple[T]( T, TypeList[T] ) }
//
// Start with option type
case class TypeVariantClause( val name : String, val elTypes : List[ExprType], val enum : Int ) extends ExprType
case class TypeVariant( val variants : List[TypeVariantClause] ) extends ExprType


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
                case TypeNone                               =>
                case TypeUnit                               =>
                case TypeFloat                              =>
                case TypeBoolean                            =>
                case TypeInteger                            =>
                case TypeString                             =>
                case TypeList(elType)                       => rec(elType);
                case TypeFunction(argTypes, retType)        => argTypes.foreach( x => rec(x) ); rec(retType);
                case TypeGeneric(id)                        =>
                case TypeReference(destName, destType)      => // Do not follow type references
                case TypeVariantClause(name, elTypes, enum) => elTypes.foreach( x => rec(x) );
                case TypeVariant(variants)                  => variants.foreach( x => rec(x) );
            }
            visitor.after(exprType)
        }
        rec(exprType)
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
                def pr( s : String ) = println( ("T "*indent) + s )
                
                exprType match
                {
                    case TypeNone                               => pr( "Untyped" )
                    case TypeUnit                               => pr( "Unit" )
                    case TypeFloat                              => pr( "Float" )
                    case TypeBoolean                            => pr( "Boolean" )
                    case TypeInteger                            => pr( "Int" )
                    case TypeString                             => pr( "String" )
                    case TypeList(elType)                       => pr( "List" )
                    case TypeFunction(argTypes, retType)        => pr( "Function" )
                    case TypeGeneric(id)                        => pr( "Generic" )
                    case TypeReference(destName, destType)      => pr( "TypeReference: " + destName )
                    case TypeVariantClause(name, elTypes, enum) => pr( "Clause: " + name )
                    case TypeVariant(variants)                  => pr( "Variant" )
                
                }
                indent += 1
            }
            override def after( exprType : ExprType )
            {
                indent -= 1
            }
        }
        VisitTypes( exprType, new Dumper() )
    }
}


