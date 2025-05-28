package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Token._
import TokenKind._

import scallion._

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[String] = accept(OperatorKind(string)) { case OperatorToken(name) => name }
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ "{" ~ many(definition) ~ opt(expr) ~ "}").map {
    case obj ~ id ~ _ ~ defs ~ body ~ _ => ModuleDef(id, defs.toList, body).setPos(obj)
  }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] = functionDefinition | caseClassDefinition | abstractClassDefinition

  // Added: type parameters parsing for definitions.
  // An abstract class definition.
  lazy val abstractClassDefinition: Syntax[ClassOrFunDef] = (kw("abstract") ~ kw("class") ~ identifier ~ opt(typeParameters)).map {
    case kw ~ _ ~ id ~ Some(tparams) => AbstractClassDef(id, tparams).setPos(kw)
    case kw ~ _ ~ id ~ None => AbstractClassDef(id, Nil).setPos(kw)
  }

  // A function definition.
  lazy val functionDefinition: Syntax[ClassOrFunDef] = (kw("def") ~ identifier ~ opt(typeParameters) ~ delimiter("(") ~ parameters ~ delimiter(")") ~ delimiter(":") ~ typeTree ~ delimiter("=") ~ delimiter("{") ~ expr ~ delimiter("}")).map {
    case kw ~ id ~ Some(tparams) ~ _ ~ params ~ _ ~ _ ~ retType ~ _ ~ _ ~ body ~ _ => FunDef(id, tparams, params, retType, body).setPos(kw)
    case kw ~ id ~ None ~ _ ~ params ~ _ ~ _ ~ retType ~ _ ~ _ ~ body ~ _ => FunDef(id, Nil, params, retType, body).setPos(kw)

  }

  // A case class definition. For now we will ignore type parameters of parent class
  lazy val caseClassDefinition: Syntax[ClassOrFunDef] = (kw("case") ~ kw("class") ~ identifier ~ opt(typeParameters) ~ delimiter("(") ~ typeParams ~ delimiter(")") ~ kw("extends") ~ identifier ~ opt(typeParameters)).map {
    case kw1 ~ _ ~ id ~ Some(tparams) ~ _ ~ fields ~ _ ~ kw2 ~ parent ~ _ => CaseClassDef(id, tparams, fields, parent).setPos(kw1)
    case kw1 ~ _ ~ id ~ None ~ _ ~ fields ~ _ ~ kw2 ~ parent ~ _ => CaseClassDef(id, Nil, fields, parent).setPos(kw1)
  }

  lazy val typeParams: Syntax[List[TypeTree]] = repsep(identifier.skip ~ delimiter(":").skip ~ typeTree, ",").map(_.toList)

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] = (identifier ~ delimiter(":") ~ typeTree).map {
    case id ~ _ ~ tt => ParamDef(id, tt)
  }

  lazy val typeParameters: Syntax[List[TypeTree]] = (delimiter("[") ~ repsep(typeTree, ",") ~ delimiter("]")).map {
    case _ ~ params ~ _ => params.toList
  }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = recursive { primitiveType | identifierType }

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  }

  // Added: type arguments for user-defined types.
  // A user-defined type (such as `List`). 
  lazy val identifierType: Syntax[TypeTree] = (identifierPos ~ opt(delimiter(".") ~ identifier) ~ opt(typeArguments)).map {
    case (module, pos) ~ Some(_ ~ name) ~ Some(targs) => TypeTree(ClassType(QualifiedName(Some(module), name), targs)).setPos(pos)
    case (name, pos) ~ None ~ Some(targs) => TypeTree(ClassType(QualifiedName(None, name), targs)).setPos(pos)
    case (module, pos) ~ Some(_ ~ name) ~ None => TypeTree(ClassType(QualifiedName(Some(module), name))).setPos(pos)
    case (name, pos) ~ None ~ None => TypeTree(ClassType(QualifiedName(None, name))).setPos(pos)
  }
  
  // New: A list of type arguments for a user-defined type.
  lazy val typeArguments: Syntax[List[TypeTree]] = (delimiter("[") ~ repsep(typeTree, ",") ~ delimiter("]")).map {
    case _ ~ args ~ _ => args.toList
  }

  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence
  lazy val expr: Syntax[Expr] = recursive { valueExpr | sequenceExpr }

  lazy val valueExpr: Syntax[Expr] = (kw("val") ~ parameter ~ delimiter("=") ~ condExpr ~ delimiter(";") ~ expr).map {
    case kw ~ df ~ _ ~ value ~ _ ~ body => Let(df, value, body).setPos(kw)
  }

  lazy val sequenceExpr: Syntax[Expr] = (condExpr ~ opt(delimiter(";") ~ expr)).map {
    case e1 ~ Some(_ ~ e2) => Sequence(e1, e2).setPos(e1)
    case e1 ~ None => e1
  }

  lazy val condExpr: Syntax[Expr] = ((ifExpr | binaryExpr) ~ many(kw("match").skip ~ delimiter("{").skip ~ many1(matchCase) ~ delimiter("}").skip)).map {
    case e ~ matches => 
      if matches.isEmpty then e
      else matches.foldLeft(e)((e, cases) => Match(e, cases.toList))
  }

  lazy val ifExpr: Syntax[Expr] = (kw("if") ~ delimiter("(") ~ expr ~ delimiter(")") ~ delimiter("{") ~ expr ~ delimiter("}") ~ kw("else") ~ delimiter("{") ~ expr ~ delimiter("}")).map {
    case kw1 ~ _ ~ cond ~ _ ~ _ ~ thenn ~ _ ~ kw2 ~ _ ~ elze ~ _ => Ite(cond, thenn, elze).setPos(kw1)
  }

  lazy val matchCase: Syntax[MatchCase] = (kw("case") ~ pattern ~ delimiter("=>") ~ expr).map {
    case kw ~ pat ~ _ ~ expr => MatchCase(pat, expr).setPos(kw)
  }

  lazy val binaryExpr: Syntax[Expr] = recursive {
    operators(unaryExpr)(
      op("*") | op("/") | op("%") is LeftAssociative,
      op("+") | op("-") | op("++") is LeftAssociative,
      op("<") | op("<=") is LeftAssociative,
      op("==") is LeftAssociative,
      op("&&") is LeftAssociative,
      op("||") is LeftAssociative
    ) {
      case (e1, "*", e2)  => Times(e1, e2)
      case (e1, "/", e2)  => Div(e1, e2)
      case (e1, "%", e2)  => Mod(e1, e2)
      case (e1, "+", e2)  => Plus(e1, e2)
      case (e1, "-", e2)  => Minus(e1, e2)
      case (e1, "++", e2) => Concat(e1, e2)
      case (e1, "<=", e2) => LessEquals(e1, e2)
      case (e1, "<", e2)  => LessThan(e1, e2)
      case (e1, "==", e2) => Equals(e1, e2)
      case (e1, "&&", e2) => And(e1, e2)
      case (e1, "||", e2) => Or(e1, e2)
    }
  }

  lazy val unaryExpr: Syntax[Expr] = (opt(op("-") | op("!")) ~ simpleExpr).map {
    case Some("-") ~ e => Neg(e)
    case Some("!") ~ e => Not(e)
    case None ~ e => e
  }

  // A literal expression.
  lazy val literal: Syntax[Literal[_]] = accept(LiteralKind) {
    case BoolLitToken(value) => BooleanLiteral(value)
    case IntLitToken(value)  => IntLiteral(value)
    case StringLitToken(value) => StringLiteral(value)
  }

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    wildPattern | literalPattern | idClassPattern
  }

  lazy val wildPattern: Syntax[Pattern] = kw("_").map {
    case kw => WildcardPattern()
  }

  lazy val literalPattern: Syntax[Pattern] = (literal | unitLiteral).map {
    case lit => LiteralPattern(lit)
  }

  // A unit literal is a special case of a literal pattern.
  lazy val unitLiteral: Syntax[Literal[_]] = (delimiter("(") ~ delimiter(")")).map {
    case _ ~ _ => UnitLiteral()
  }

  lazy val idClassPattern: Syntax[Pattern] = (identifierPos ~ opt(delimiter(".").skip ~ identifier) ~ opt(delimiter("(") ~ repsep(pattern, ",") ~ delimiter(")"))).map {
    case (module, pos) ~ Some(name) ~ Some(_ ~ args ~ _) => CaseClassPattern(QualifiedName(Some(module), name), args.toList).setPos(pos)
    case (name, pos) ~ None ~ Some(_ ~ args ~ _) => CaseClassPattern(QualifiedName(None, name), args.toList).setPos(pos)
    case (name, pos) ~ None ~ None => IdPattern(name).setPos(pos)
  }

  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] = literal.up[Expr] | variableOrCall | errorExpr | bracketExpr

  lazy val variableOrCall: Syntax[Expr] = (identifier ~ opt(delimiter(".") ~ identifier) ~ opt(delimiter("(") ~ arguments ~ delimiter(")"))).map {
    case id1 ~ Some(_ ~ id2) ~ Some(_ ~ params ~ _) => Call(QualifiedName(Some(id1), id2), params)
    case id1 ~ None ~ Some(_ ~ params ~ _) => Call(QualifiedName(None, id1), params)
    case id1 ~ None ~ None => Variable(id1)
    case _ => throw new AmycFatalError("match error : variableOrCall")
  }

  // A list of arguments for a function call.
  lazy val arguments: Syntax[List[Expr]] = repsep(expr, ",").map(_.toList)

  lazy val errorExpr: Syntax[Expr] = (kw("error") ~ delimiter("(") ~ expr ~ delimiter(")")).map {
    case kw ~ _ ~ e ~ _ => Error(e)
  }

  lazy val bracketExpr: Syntax[Expr] = (delimiter("(") ~ opt(expr) ~ delimiter(")")).map {
    case _ ~ Some(e) ~ _ => e
    case _ ~ None ~ _ => UnitLiteral()
  }


  // TODO: Other definitions.
  //       Feel free to decompose the rules in whatever way convenient.


  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}
