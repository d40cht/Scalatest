package org.seacourt.pacatoon

import scala.collection.{mutable, immutable}
import scala.util.parsing.combinator._


class ParserError( msg : String ) extends RuntimeException(msg)

object CalculatorDSL extends RegexParsers with PackratParsers
{
    // TODO: Sort keyword parsing out to avoid having the minging '@' prefix
    def ident: Parser[String] = """[a-zA-Z_]\w*""".r
    def wholeNumber: Parser[String] = """-?\d+""".r
    def decimalNumber: Parser[String] = """(\d+(\.\d*)?|\d*\.\d+)""".r
    def stringLiteral: Parser[String] = ("\""+"""([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"").r
    def floatingPointNumber: Parser[String] = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    
    def comment: Parser[Expression] = """//[^\r\n]*""".r ^^ { x => new NullExpression() }
    
    
    def buildApply( initial : Expression, terms : List[Expression] ) =
    {
        terms.foldLeft( initial )( (lhs, rhs) =>
        {
            val app = new Apply( lhs, rhs )
            app.setPos(lhs.pos)
            app
        } )
    }
    
    lazy val expr : Parser[Expression] = positioned(term4 ~ ((term4)*) ^^ {
        case x ~ Nil    =>  x
        case x ~ y      => buildApply(x, y) })
        
    lazy val term4: Parser[Expression] = positioned(term3 ~ ((("&&"|"||") ~ expr)?) ^^
    {
        case e ~ None => e
        case l ~ Some("&&" ~ r)     => new LogicalAnd( l, r )
        case l ~ Some("||" ~ r)     => new LogicalOr( l, r )
    })
    
    lazy val term3: Parser[Expression] = positioned(term2 ~ ((("<="|">="|"=="|"!="|"<"|">") ~ expr)?) ^^
    {
        case e ~ None => e
        case l ~ Some("<=" ~ r)     => new CmpLe( l, r )
        case l ~ Some(">=" ~ r)     => new CmpGe( l, r )
        case l ~ Some("==" ~ r)     => new CmpEq( l, r )
        case l ~ Some("!=" ~ r)     => new CmpNe( l, r )
        case l ~ Some("<" ~ r)      => new CmpLt( l, r )
        case l ~ Some(">" ~ r)      => new CmpGt( l, r )
    })
    
    lazy val term2: Parser[Expression] = positioned(term1 ~ ((("+"|"-") ~ term2)?) ^^ {
        case e ~ None => e
        case l ~ Some("+" ~ r)      => new Addition( l, r )
        case l ~ Some("-" ~ r)      => new Subtraction( l, r )
    })
    
    lazy val term1: Parser[Expression] = positioned(term0 ~ ((("*"|"/") ~ term1)?) ^^ {
        case e ~ None => e
        case l ~ Some("*" ~ r)      => new Multiplication( l, r )
        case l ~ Some("/" ~ r)      => new Division( l, r )
    })
    
    lazy val term0: Parser[Expression] = positioned(factor ~ (("::" ~ term1)?) ^^ {
        case e ~ None => e
        case l ~ Some("::" ~ r)     => new ListAppend( l, r )
    })
    
    lazy val idExpression : Parser[Expression] = positioned(ident ^^ { x => new IdExpression(x) })
    
    lazy val factor: Parser[Expression] = positioned(blockScope | controlFlow | typeAnnotation | typeDefn | defn | fpLit | stringLit | "(" ~> expr <~ ")" ^^ { e => e } | idExpression ^^ { e => e })
    
    lazy val fpLit : Parser[Expression] = positioned(floatingPointNumber ^^ 
    {
        lit =>
        {
            val isInteger = !lit.foldLeft(false)((x,y) => x || (y=='.'))
            if ( isInteger )
            {
                val c = new Constant( new IntegerValue(lit.toInt) )
                c.exprType = new TypeInteger()
                c
            }
            else
            {
                val c = new Constant( new FloatValue(lit.toDouble) )
                c.exprType = new TypeFloat()
                c
            }
        } 
    })
    
    lazy val stringLit : Parser[Expression] = positioned(stringLiteral ^^ {
        str =>
        {
            val c = new Constant( new StringValue( str.drop(1).dropRight(1) ) )
            c.exprType = new TypeString()
            c
        }
    })
 
    lazy val defn : Parser[Expression] = positioned("@def" ~ ident ~ ((ident)*) ~ "=" ~ expr ^^ {
        case "@def" ~ id ~ params ~ "=" ~ e => new IdDefinition( id, params, e )
    })
    
    lazy val namedType : Parser[Expression] = ident ^^ { x => new NamedTypeExpr(x) }
    lazy val listType : Parser[Expression] = "[" ~> typeExpr <~ "]" ^^ { x => new ListTypeExpr(x) }
    lazy val subType : Parser[Expression] = "(" ~> typeExpr <~ ")" ^^ { x => x }
    
    lazy val typeEl : Parser[Expression] = (namedType | listType | subType) ^^ { x => x }
    lazy val typeExpr : Parser[Expression] = typeEl ~ (("->" ~> typeEl)*) ^^ {
        case first ~ second => new TypeExpr( first :: second )
    }
    
    lazy val typeAnnotation : Parser[Expression] = positioned("@def" ~ ident ~ "::" ~ typeExpr ^^ {
        case "@def" ~ id ~ "::" ~ typeExpr => new TypeAnnotation( id, typeExpr )
    })
    
    lazy val variantClause : Parser[VariantClauseDefinition] = positioned(ident ~ (ident*) ^^
    {
        case clauseName ~ elementTypeNames => new VariantClauseDefinition( clauseName, elementTypeNames )
    })
    
    lazy val variantTypeDefn : Parser[VariantTypeDefinition] = positioned(variantClause ~ (("|" ~> variantClause)*) ^^
    {
        case firstAlternative ~ otherAlternatives => new VariantTypeDefinition( firstAlternative ::otherAlternatives)
    })
    
    lazy val typeDefn : Parser[Expression] = positioned("@type" ~ ident ~ ((ident)*) ~ "=" ~ variantTypeDefn ^^
    {
        case "@type" ~ typeName ~ typeParameters ~ "=" ~ instanceType => new TypeDefinition( typeName, typeParameters, instanceType )
    } )
    
    lazy val topLevel = positioned(comment | expr ^^ { x => x })
    
    lazy val exprList : Parser[ExprList] = positioned(topLevel ~ ((((";")?) ~ exprList)?) ^^ {
        case e ~ None           => new ExprList( e :: Nil )
        case e ~ Some(_ ~ eL)   => new ExprList( e :: eL.elements )
    })
    
    lazy val controlFlow : Parser[Expression] = positioned("@if" ~ "(" ~ expr ~ ")" ~ expr ~ (("@else" ~ expr)?) ^^
    {
        case "@if" ~ "(" ~ cond ~ ")" ~ trueBranch ~ None                           => new IfExpression( cond, trueBranch, new NullExpression() )
        case "@if" ~ "(" ~ cond ~ ")" ~ trueBranch ~ Some("@else" ~ falseBranch)    => new IfExpression( cond, trueBranch, falseBranch )
    })
    
    lazy val blockScope : Parser[Expression] = positioned("{" ~> exprList <~ "}" ^^ { e => new BlockScopeExpression( e ) })

    def parse( expression : String ) =
    {
        parseAll( exprList, expression ) match
        {
            case NoSuccess( msg, next ) => throw new ParserError( "(line " + next.pos.line + ", column " + next.pos.column + "): " + msg )
            case Success( ast, next ) => ast
        }
    }
}


