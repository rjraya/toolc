package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import grammarcomp.grammar._
import GrammarUtils.InLL1
import CFGrammar._
import grammarcomp.parsing._
import GrammarDSL._

object Parser extends Pipeline[Iterator[Token], Program] {

  val toolGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,

    'Type ::= INT() ~ LBRACKET() ~ RBRACKET() | BOOLEAN() | INT() | STRING() | 'Identifier,

    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),

    'Expression ::= 'Expression ~ 'Op ~ 'Expression
      | 'Expression ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | 'Expression ~ DOT() ~ LENGTH()
      | 'Expression ~ DOT() ~ 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN()
      | INTLITSENT | STRINGLITSENT
      | TRUE() | FALSE() | 'Identifier | THIS()
      | NEW() ~ INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET()
      | NEW() ~ 'Identifier ~ LPAREN() ~ RPAREN()
      | BANG() ~ 'Expression
      | LPAREN() ~ 'Expression ~ RPAREN(),
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList,
    'Op ::= AND() | OR() | EQUALS() | LESSTHAN() | PLUS() | MINUS() | TIMES() | DIV(),
    'Identifier ::= IDSENT
  ))

  // TODO: Transform this to an LL(1) grammar
  val ll1Grammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'MainObject ~ 'ClassDecls ~ EOF(),
    'MainObject ::= PROGRAM() ~ 'Identifier ~ LBRACE() ~ 'Stmts ~ RBRACE(),
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    'ClassDeclaration ::= CLASS() ~ 'Identifier ~ 'OptExtends ~ 'ClassBody,
    'OptExtends ::= epsilon() | EXTENDS() ~ 'Identifier,
    'ClassBody ::= LBRACE() ~ 'VarDecs ~ 'MethodDecs ~ RBRACE(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(),
    'VarDeclaration ::= VAR() ~ 'Param ~ SEMICOLON(),
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'MethodDeclaration ::= DEF() ~ 'Identifier ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'VarDecs ~ 'Stmts ~ RETURN() ~ 'Expression ~ SEMICOLON() ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Identifier ~ COLON() ~ 'Type,

    'Type ::= INT() ~ 'TypeInt | BOOLEAN() | STRING() | 'Identifier,
    'TypeInt ::= LBRACKET() ~ RBRACKET() | epsilon(),

    'Statement ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ 'ElseOpt
      | 'SimpleStat,
    'MatchedIf ::= IF() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf ~ ELSE() ~ 'MatchedIf
      | 'SimpleStat,
    'SimpleStat ::= LBRACE() ~ 'Stmts ~ RBRACE()
      | WHILE() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ 'MatchedIf
      | PRINTLN() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON()
      | 'Identifier ~ 'IdStat
      | DO() ~ LPAREN() ~ 'Expression ~ RPAREN() ~ SEMICOLON(),
    'IdStat ::= EQSIGN() ~ 'Expression ~ SEMICOLON()
      | LBRACKET() ~ 'Expression ~ RBRACKET() ~ EQSIGN() ~ 'Expression ~ SEMICOLON(),
    'ElseOpt ::= ELSE() ~ 'Statement | epsilon(),

    'Expression ::= 'OrExpr,
    'OrExpr ::= 'AndExpr ~ 'OrExprP,
    'OrExprP ::= epsilon() | OR() ~ 'OrExpr,
    'AndExpr ::= 'LessOrEqual ~ 'AndExprP,
    'AndExprP ::= epsilon() | AND() ~ 'AndExpr,
    'LessOrEqual ::= 'PlusOrMinus ~  'LessOrEqualP,
    'LessOrEqualP ::= LESSTHAN() ~ 'LessOrEqual | EQUALS() ~ 'LessOrEqual | epsilon(),
    'PlusOrMinus ::= 'TimesOrDiv ~ 'PlusOrMinusP,
    'PlusOrMinusP ::= PLUS() ~ 'PlusOrMinus | MINUS() ~ 'PlusOrMinus | epsilon(),
    'TimesOrDiv ::= 'BangExpr ~  'TimesOrDivP,
    'TimesOrDivP ::= TIMES() ~ 'TimesOrDiv | DIV() ~ 'TimesOrDiv | epsilon(),
    'BangExpr ::= BANG() ~ 'BangExpr | 'FinalExpr,
    'FinalExpr ::= 'BasicExpr ~ 'Recursion,
    'BasicExpr ::= TRUE() | FALSE() | INTLITSENT | STRINGLITSENT | THIS() | 'Identifier | NEW() ~ 'NewExpr  | LPAREN() ~ 'Expression ~ RPAREN(),
    'Recursion ::= epsilon() | LBRACKET() ~ 'Expression ~ RBRACKET() ~ 'Recursion | DOT() ~ 'DotExpr ~ 'Recursion,
    'NewExpr ::= INT() ~ LBRACKET() ~ 'Expression ~ RBRACKET() | 'Identifier ~ LPAREN() ~ RPAREN(),
    'DotExpr ::= LENGTH()  | 'Identifier ~ LPAREN() ~ 'Args ~ RPAREN(),

    'Identifier ::= IDSENT,
    'Args ::= epsilon() | 'Expression ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expression ~ 'ExprList
  ))

  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()
    val list = tokens.toList
    GrammarUtils.isLL1WithFeedback(ll1Grammar) match {
      case InLL1() =>
        info("Grammar is in LL1")
      case other =>
        warning(other)
    }
    val feedback = ParseTreeUtils.parseWithTrees(ll1Grammar, list)
    feedback match {
      case s: Success[Token] =>
        (new ASTConstructorLL1).constructProgram(s.parseTrees.head)
      case fdb =>
        fatal("Parsing failed: "+fdb)
    }


  }

}
