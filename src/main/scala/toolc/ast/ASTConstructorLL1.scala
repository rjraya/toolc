package toolc
package ast

import ast.Trees.{ExprTree, _}
import grammarcomp.grammar.GrammarDSL._
import lexer.Token
import lexer.Tokens._
import grammarcomp.parsing._

class ASTConstructorLL1 extends ASTConstructor {

  override def constructType(ptree: NodeOrLeaf[Token]): TypeTree = {
    ptree match {
      case Node('Type ::= List(INT(),'TypeInt), List(Leaf(i@INT()),Node('TypeInt ::= _,List()))) =>
        IntType().setPos(i)
      case Node('Type ::= List(INT(),'TypeInt), List(Leaf(i@INT()),Node('TypeInt ::= _,List(Leaf(lb@LBRACKET()),_)))) =>
        IntArrayType().setPos(i)
      case Node('Type ::= _, List(Leaf(b@BOOLEAN()))) =>
        BooleanType().setPos(b)
      case Node('Type ::= _, List(Leaf(s@STRING()))) =>
        StringType().setPos(s)
      case Node('Type ::= List('Identifier), List(id)) =>
        val pid = constructId(id)
        ClassType(pid).setPos(pid)
    }
  }

 override def constructExpr(ptree: NodeOrLeaf[Token]): ExprTree = {
   ptree match {
     case Node('Expression ::= _, List(orexpr)) =>
       constructExpr(orexpr)
     case Node('OrExpr ::= _, List(andexpr,Node('OrExprP ::= _, List(l@Leaf(OR()),orexpr))) )  =>
       associateLeft(andexpr,orexpr,convertOp(l))
     case Node('OrExpr ::= _, List(andexpr,Node('OrExprP ::= _, List())))  =>
       constructExpr(andexpr)

     case Node('AndExpr ::= _, List(lessorequalexpr,Node('AndExprP ::= _, List(l@Leaf(AND()),andexpr))))  =>
       associateLeft(lessorequalexpr,andexpr,convertOp(l))
     case Node('AndExpr ::= _, List(lessorequalexpr,Node('AndExprP ::= _, List())))  =>
       constructExpr(lessorequalexpr)
     case Node('LessOrEqual ::= _, List(plusorminusexpr,Node('LessOrEqualP ::= _, List(l@Leaf(LESSTHAN()),lessorequalexpr))))  =>
       associateLeft(plusorminusexpr,lessorequalexpr,convertOp(l))
     case Node('LessOrEqual ::= _, List(plusorminusexpr,Node('LessOrEqualP ::= _, List(l@Leaf(EQUALS()),lessorequalexpr))))  =>
       associateLeft(plusorminusexpr,lessorequalexpr,convertOp(l))
     case Node('LessOrEqual ::= _, List(plusorminusexpr,Node('LessOrEqualP ::= _, List())))  =>
       constructExpr(plusorminusexpr)
     case Node('PlusOrMinus ::= _, List(timesordivexpr,Node('PlusOrMinusP ::= _, List(l@Leaf(PLUS()),plusorminusexpr))))  =>
       associateLeft(timesordivexpr,plusorminusexpr,convertOp(l))
     case Node('PlusOrMinus ::= _, List(timesordivexpr,Node('PlusOrMinusP ::= _, List(l@Leaf(MINUS()),plusorminusexpr))))  =>
       associateLeft(timesordivexpr,plusorminusexpr,convertOp(l))
     case Node('PlusOrMinus ::= _, List(timesordivexpr,Node('PlusOrMinusP ::= _, List())))  =>
       constructExpr(timesordivexpr)
     case Node('TimesOrDiv ::= _, List(finalexpr,Node('TimesOrDivP ::= _, List(l@Leaf(TIMES()),timesordivexpr))))  =>
       associateLeft(finalexpr,timesordivexpr,convertOp(l))
     case Node('TimesOrDiv ::= _, List(finalexpr,Node('TimesOrDivP ::= _, List(l@Leaf(DIV()),timesordivexpr))))  =>
       associateLeft(finalexpr,timesordivexpr,convertOp(l))
     case Node('TimesOrDiv ::= _, List(finalexpr,Node('TimesOrDivP ::= _, List())))  =>
       constructExpr(finalexpr)

     case Node('BasicExpr ::= _, List(Leaf(tt@TRUE()))) =>
        True().setPos(tt)
     case Node('BasicExpr ::= _, List(Leaf(tf@FALSE()))) =>
       False().setPos(tf)
     case Node('BasicExpr ::=  _, List(Leaf(tt@THIS()))) =>
       This().setPos(tt)
     case Node('BasicExpr ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
       IntLit(i).setPos(it)
     case Node('BasicExpr ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
       StringLit(s).setPos(st)
     case Node('BangExpr ::= List(BANG(), 'BangExpr), List(Leaf(bt), e)) =>
       Not(constructExpr(e)).setPos(bt)  //check that this call is ok
     case Node('BangExpr ::= List('FinalExpr), List(fexpr))  =>
       constructExpr(fexpr)
     case Node('BasicExpr ::= List('Identifier), List(id)) =>
       val pid = constructId(id)
       Variable(pid).setPos(pid)
     case Node('BasicExpr ::= List(NEW(),'NewExpr),List(Leaf(nt),newexpression))  =>
       constructExpr(newexpression).setPos(nt)
     case Node('NewExpr ::= List(INT(), LBRACKET(), 'Expression, RBRACKET()), List( _, _, e, _)) =>
       NewIntArray(constructExpr(e))
     case Node('NewExpr ::= List('Identifier, LPAREN(), RPAREN()), List(id,_,_)) =>
       New(constructId(id))
     case Node('BasicExpr ::= List(LPAREN(), 'Expression, RPAREN()), List(Leaf(lp), e, _)) =>
       constructExpr(e).setPos(lp)

     case Node('FinalExpr ::= _, List(basicexpr,Node('Recursion ::= _, List(Leaf(lb@LBRACKET()),e,_,recursionexpr))))  =>
       val pe1 = constructExpr(basicexpr)
       val pe2 = constructExpr(e)
       val pe3 = ArrayRead(pe1, pe2).setPos(pe1)
       constructRecExpr(pe3,recursionexpr) //set pos
     case Node('FinalExpr ::= _, List(basicexpr,Node('Recursion ::= _, List( Leaf(dot@DOT()),Node('DotExpr ::= _,List(Leaf(le@LENGTH()))),recursionexpr )))) =>
       val pe1 = constructExpr(basicexpr)
       val pe2 = ArrayLength(pe1).setPos(pe1)
       constructRecExpr(pe2,recursionexpr) //set pos
     case Node('FinalExpr ::= _, List(basicexpr,Node('Recursion ::= _, List( Leaf(dot@DOT()),Node('DotExpr ::= _,List(id,_,as,_)) ,recursionexpr))))  =>
       val pe1 = constructExpr(basicexpr)
       val pe2 = MethodCall(pe1, constructId(id), constructList(as, constructExpr, hasComma = true)).setPos(pe1)
       constructRecExpr(pe2,recursionexpr) //set pos
     case Node('FinalExpr ::= _, List(basicexpr,recursionexpr@Node('Recursion ::= _, List())))  =>
       val pe1 = constructExpr(basicexpr)
       constructRecExpr(pe1,recursionexpr) //set pos

   }
 }

  def constructRecExpr(son: ExprTree,ptree: NodeOrLeaf[Token]): ExprTree = {
    ptree match {
      case Node('Recursion ::= _, List(Leaf(lb@LBRACKET()),e,_,recursionexpr)) =>
        val pe1 = constructExpr(e)
        val pe2 = ArrayRead(son, pe1).setPos(son)
        constructRecExpr(pe2,recursionexpr)
      case Node('Recursion ::= _, List(Leaf(dot@DOT()), Node('DotExpr ::= _, List(Leaf(le@LENGTH()))), recursionexpr)) =>
        val pe1 = ArrayLength(son).setPos(son)
        constructRecExpr(pe1,recursionexpr)
      case Node('Recursion ::= _, List(Leaf(dot@DOT()), Node('DotExpr ::= _, List(id, _, as, _)), recursionexpr)) =>
        val pe1 = MethodCall(son, constructId(id), constructList(as, constructExpr, hasComma = true)).setPos(son)
        constructRecExpr(pe1,recursionexpr)
      case Node('Recursion ::= List(), _) =>
        son
    }
  }

  def associateLeft(lexpr : NodeOrLeaf[Token],rexpr : NodeOrLeaf[Token],op : (ExprTree,ExprTree) => ExprTree) :ExprTree = {
    val pe1 = constructExpr(lexpr)
    val leafoperator = convertToLeaf(op)
    val complementoperator = complementOperator(op)
    val leafcomplementoperator = convertToLeaf(complementoperator)
    rexpr match {
      case Leaf(t) =>
        op(pe1, constructExpr(rexpr)).setPos(pe1)
      case Node(_,List(Leaf(t),rp)) if (Leaf(t) == leafoperator || Leaf(t) == leafcomplementoperator) =>
        if(Leaf(t) == leafoperator){
          associateLeftExpr(pe1,rp,op)
        } else {
          associateLeftExpr(pe1,rp,complementoperator)
        }
      case Node(_, List(l,rp)) =>
        associateLeftExpr(op(pe1, constructExpr(l)).setPos(pe1),rp,op)
      case Node(_,List()) =>
        pe1
    }
  }

  def associateLeftExpr(lexpr : ExprTree,rexpr : NodeOrLeaf[Token],op : (ExprTree,ExprTree) => ExprTree) :ExprTree = {
    val leafoperator = convertToLeaf(op)
    val complementoperator = complementOperator(op)
    val leafcomplementoperator = convertToLeaf(complementoperator)
    rexpr match {
      case Leaf(t) =>
        op(lexpr, constructExpr(rexpr)).setPos(lexpr)
      case Node(_,List(Leaf(t),rp)) if (Leaf(t) == leafoperator || Leaf(t) == leafcomplementoperator)  =>
        if(Leaf(t) == leafoperator){
          associateLeftExpr(lexpr,rp,op)
        } else {
          associateLeftExpr(lexpr,rp,complementoperator)
        }
      case Node(_, List(l,rp)) =>
        associateLeftExpr(op(lexpr, constructExpr(l)).setPos(lexpr),rp,op)
      case Node(_,List())  =>
        lexpr
    }
  }

  def convertOp(ptree: NodeOrLeaf[Token]): (ExprTree, ExprTree) => ExprTree = {
    ptree match {
      case Leaf(t) => (t: @unchecked) match {
        case AND()      => And
        case OR()       => Or
        case EQUALS()   => Equals
        case LESSTHAN() => LessThan
        case PLUS()     => Plus
        case MINUS()    => Minus
        case TIMES()    => Times
        case DIV()      => Div
      }
    }
  }

  def convertToLeaf(op:(ExprTree, ExprTree) => ExprTree) : NodeOrLeaf[Token] = {
    op match {
      case And        => Leaf(AND())
      case Or         => Leaf(OR())
      case Equals     => Leaf(EQUALS())
      case LessThan   => Leaf(LESSTHAN())
      case Plus       => Leaf(PLUS())
      case Minus      => Leaf(MINUS())
      case Times      => Leaf(TIMES())
      case Div        => Leaf(DIV())
    }
  }

  def complementOperator(op:(ExprTree, ExprTree) => ExprTree) : (ExprTree, ExprTree) => ExprTree  = {
    op match {
      case LessThan => Equals
      case Equals => LessThan
      case Plus => Minus
      case Minus => Plus
      case Times => Div
      case Div => Times
      case _ => op
    }
  }

}