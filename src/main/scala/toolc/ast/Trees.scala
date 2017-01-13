package toolc
package ast

import utils._
import analyzer.Symbols._
import analyzer.Types._

object Trees {
  sealed trait Tree extends Positioned

  // Identifiers represent names in Tool. When a unique symbol gets attached to them,
  // they become unique
  case class Identifier(value: String) extends Tree with Symbolic[Symbol] with Typed {
    override def getType: Type = getSymbol match {
      case cs: ClassSymbol => TClass(cs)
      case ms: MethodSymbol => sys.error("Requesting type of a method identifier.")
      case ms: MainSymbol => sys.error("Requesting type of main object")
      case vs: VariableSymbol => vs.getType
    }
    override def toString = value
  }

  // Definitions
  sealed trait DefTree extends Tree
  case class Program(main: MainObject, classes: List[ClassDecl])
    extends DefTree
  case class MainObject(id: Identifier, stats: List[StatTree])
    extends DefTree with Symbolic[MainSymbol]
  case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
    extends DefTree with Symbolic[ClassSymbol]
  case class VarDecl(tpe: TypeTree, id: Identifier)
    extends DefTree with Symbolic[VariableSymbol]
  case class MethodDecl(id: Identifier, args: List[Formal], retType: TypeTree, vars: List[VarDecl], stats: List[StatTree], retExpr: ExprTree)
    extends DefTree with Symbolic[MethodSymbol]
  case class Formal(tpe: TypeTree, id: Identifier)
    extends DefTree with Symbolic[VariableSymbol]

  // Types
  sealed trait TypeTree extends Tree with Typed
  case class IntArrayType() extends TypeTree { override def getType = TIntArray}
  case class IntType() extends TypeTree {override def getType = TInt}
  case class BooleanType() extends TypeTree {override def getType = TBoolean}
  case class StringType() extends TypeTree {override def getType = TString}
  case class ClassType(id: Identifier) extends TypeTree {override def getType = id.getType}

  // Statements
  sealed trait StatTree extends Tree
  case class Block(stats: List[StatTree]) extends StatTree
  case class If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) extends StatTree
  case class While(expr: ExprTree, stat: StatTree) extends StatTree
  case class Println(expr: ExprTree) extends StatTree
  case class Assign(id: Identifier, expr: ExprTree) extends StatTree
  case class ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree
  case class DoExpr(e: ExprTree) extends StatTree

  // Expressions
  sealed trait ExprTree extends Tree with Typed
  sealed trait Literal{def getType : Type}
  // Boolean operators
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree {val getType = TBoolean}
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree {val getType = TBoolean}
  case class Not(expr: ExprTree) extends ExprTree {val getType = TBoolean}
  // Arithmetic operators (Plus works on any combination of Int/String)
  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree{
    def getType = if (lhs.getType == TString || rhs.getType == TString) TString else TInt
  }
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree {val getType = TInt}
  case class Times(lhs: ExprTree, rhs: ExprTree) extends ExprTree {val getType = TInt}
  case class Div(lhs: ExprTree, rhs: ExprTree) extends ExprTree {val getType = TInt}
  case class LessThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree {val getType = TBoolean}
  // Equality
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree {val getType = TBoolean}
  // Array expressions
  case class ArrayRead(arr: ExprTree, index: ExprTree) extends ExprTree {val getType = TInt}
  case class ArrayLength(arr: ExprTree) extends ExprTree {val getType = TInt}
  case class NewIntArray(size: ExprTree) extends ExprTree {val getType = TIntArray}
  // Object-oriented expressions
  case class This() extends ExprTree with Symbolic[ClassSymbol] with Literal {def getType = TClass(getSymbol)}
  case class MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) extends ExprTree {
    def getType = obj.getType match{
      case TClass(cs) => cs.lookupMethod(meth.value) match{
        case Some(ms) => ms.getType
        case _ => TError
      }
    }

  }
  case class New(tpe: Identifier) extends ExprTree {
    def getType = tpe.getType match {
      case t@TClass(_) => t
      case other => TError
    }
  }
  // Literals
  case class IntLit(value: Int) extends ExprTree with Literal {val getType = TInt}
  case class StringLit(value: String) extends ExprTree with Literal {val getType = TString}
  case class True() extends ExprTree with Literal {val getType = TBoolean}
  case class False() extends ExprTree with Literal {val getType = TBoolean}
  // Variables
  case class Variable(id: Identifier) extends ExprTree {def getType = id.getType}
}
