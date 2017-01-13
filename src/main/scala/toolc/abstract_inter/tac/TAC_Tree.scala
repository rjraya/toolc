package toolc.abstract_inter.tac

import toolc.ast.Trees._
import toolc.analyzer.Types._

abstract class TAC_Instr{var index:Int}
abstract class TAC_Op{def getType : Type}

case class TAC_Label(label: String,var index : Int) extends TAC_Instr{
  override def toString : String = label + ":"
}
case class TAC_Jump(label: TAC_Label,var index : Int) extends TAC_Instr{
  override def toString : String = "jump " + label
}
case class TAC_CJump(label: TAC_Label, cond: TAC_Op,var index : Int) extends TAC_Instr{
  override def toString : String = "cjump if " + cond + " to " + label
}
case class TAC_Ret(value:TAC_Op,var index : Int) extends TAC_Instr{
  override def toString: String = "return " + value
}
case class TAC_ArrayLength(lhs: TAC_Temp, array: TAC_Op,var index : Int) extends TAC_Instr{
  override def toString: String = lhs + " = " + array + ".length"
}
case class TAC_NewClass(lhs: TAC_Temp,id:Identifier,var index : Int) extends TAC_Instr{
  override def toString : String = lhs + " = new " + id.value + "()"
}
case class TAC_NewIntArray(lhs: TAC_Temp, size : TAC_Op,var index : Int) extends TAC_Instr{
  override def toString : String = lhs + " = new Int[" + size + "]"
}

case class TAC_MethodCall(lhs: TAC_Temp, receiver: TAC_Op, meth : Identifier, paramList: List[TAC_Op],var index : Int) extends TAC_Instr{
  override def toString : String = lhs + " = " + receiver + "." + meth.value  + paramList.mkString("(",",",")")
}
case class TAC_PrintLn(param : TAC_Op,var index : Int) extends TAC_Instr{
  override def toString : String = "println" + "(" + param + ")"
}
case class TAC_DoExpr(param : TAC_Op,var index : Int) extends TAC_Instr{
  override def toString : String = "do" + "(" + param + ")"
}
case class TAC_Assign(lhs: TAC_Op, rhs: TAC_Op,var index : Int) extends TAC_Instr{
  override def toString : String = lhs + " = " + rhs
}
case class TAC_ArrayAssign(array: TAC_Var, arrindex: TAC_Op, value: TAC_Op,var index : Int) extends TAC_Instr{
  override def toString : String = array + "[" + arrindex + "]" + " = " + value
}
case class TAC_ArrayRead(lhs: TAC_Temp, array: TAC_Op, arrindex: TAC_Op,var index : Int) extends TAC_Instr{
  override def toString : String = lhs + " = " + array + "[" + arrindex + "]"
}
case class TAC_BinOp(lhs: TAC_Temp, lexpr: TAC_Op, rexpr: TAC_Op, binop: String,var index : Int) extends TAC_Instr{
  override def toString : String = lhs + " = " + lexpr + " " + binop + " " + rexpr
}
case class TAC_UnOp(lhs: TAC_Temp, rhs : TAC_Op, unop: String,var index : Int) extends TAC_Instr{
  override def toString : String = lhs + " = " + unop + rhs
}

case class TAC_Var(id : Identifier) extends TAC_Op{
  override def toString : String = id.value
  override def getType : Type = id.getType
}

case class TAC_Lit(lit_node: Literal,tpe : Type) extends TAC_Op{
  override def toString : String = lit_node.toString
  override def getType : Type = tpe
}

case class TAC_Temp(name : String, tpe : Type) extends TAC_Op{
  override def toString : String = name
  override def getType : Type = tpe
}

case class TAC_Comment(comment: String,var index : Int) extends TAC_Instr{
  override def toString : String = comment
}

class TAC_List{
  var list = List[TAC_Instr]()
  var tempCount = 0

  def add(t:TAC_Instr) = list = list :+ t

  def alloc(tpe : Type) : TAC_Temp = {
    tempCount += 1
    TAC_Temp("t"+tempCount,tpe)
  }
  def size(): Int = list.length

  def numTempVariables : Int = tempCount
}

sealed trait TAC_DefTree
case class TAC_Program(main: TAC_MainObject, classes: List[TAC_ClassDecl]) extends TAC_DefTree
case class TAC_ClassDecl(cdecl : ClassDecl, methods: List[TAC_MethodDecl]) extends TAC_DefTree
case class TAC_MethodDecl(meth : MethodDecl,var tacList : List[TAC_Instr],numTempVariables:Int) extends TAC_DefTree
case class TAC_MainObject(main: MainObject, stats: List[TAC_Instr],numTempVariables:Int) extends TAC_DefTree