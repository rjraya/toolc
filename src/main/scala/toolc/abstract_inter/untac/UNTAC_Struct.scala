package toolc.abstract_inter.untac

import toolc.analyzer.Types._
import toolc.ast.Trees._

sealed trait UNTAC_Instr{def getType : Type}
abstract class UNTAC_Op extends UNTAC_Instr

case class UNTAC_Label(label: String) extends UNTAC_Instr{
  override def toString(): String = label + ":"
  override def getType = TUntyped
}
case class UNTAC_Jump(label: UNTAC_Label) extends UNTAC_Instr{
  override def toString(): String = "jump " + label
  override def getType = TUntyped
}
case class UNTAC_CJump(label: UNTAC_Label, cond: UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = "cjump if " + cond + " to " + label
  override def getType = TUntyped
}
case class UNTAC_Ret(value:UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = "return " + value
  override def getType = TUntyped
}
case class UNTAC_ArrayLength(t:Option[UNTAC_Temp],array: UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = array + ".length"
  override def getType = TInt
}
case class UNTAC_NewClass(t:Option[UNTAC_Temp],id:Identifier) extends UNTAC_Instr{
  override def toString(): String = "new " + id.value + "()"
  override def getType = id.getType
}
case class UNTAC_NewIntArray(t:Option[UNTAC_Temp],size : UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = "new Int[" + size + "]"
  def getType = TIntArray
}

case class UNTAC_MethodCall(t:Option[UNTAC_Temp],receiver: UNTAC_Instr, meth : Identifier, paramList: List[UNTAC_Instr]) extends UNTAC_Instr{
  override def toString(): String = receiver + "." + meth.value  + paramList.mkString("(",",",")")
  def getType = meth.getSymbol.getType
}
case class UNTAC_PrintLn(param : UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = "println" + "(" + param.toString() + ")"
  override def getType = TUntyped
}
case class UNTAC_DoExpr(param : UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = "do" + "(" + param.toString() + ")"
  override def getType = TUntyped
}
case class UNTAC_Assign(t:Option[UNTAC_Temp],lhs: Option[UNTAC_Instr], rhs: UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = lhs match{
    case Some(l) => l + " = " + rhs
    case _ => sys.error("Left hand side empty.")
  }
  override def getType = TUntyped
}
case class UNTAC_ArrayAssign(array: UNTAC_Var, arrindex: UNTAC_Instr, value: UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = array + "[" + arrindex + "]" + " = " + value
  override def getType = TUntyped
}
case class UNTAC_ArrayRead(t:Option[UNTAC_Temp],array: UNTAC_Instr, arrindex: UNTAC_Instr) extends UNTAC_Instr{
  override def toString(): String = array + "[" + arrindex + "]"
  override def getType = TInt
}
case class UNTAC_BinOp(t:Option[UNTAC_Temp],lexpr: UNTAC_Instr, rexpr: UNTAC_Instr, binop: String) extends UNTAC_Instr{
  override def toString(): String = "(" + lexpr + " " + binop + " " + rexpr + ")"
  override def getType = binop match{
    case "-"|"*"|"/" => TInt
    case "+" => if (lexpr.getType == TString || rexpr.getType == TString) TString else TInt
    case "=="|"<"|"&&"|"||" => TBoolean
  }
}
case class UNTAC_UnOp(t:Option[UNTAC_Temp],rhs : UNTAC_Instr, unop: String) extends UNTAC_Instr{
  override def toString(): String = "(" + unop + rhs + ")"
  override def getType = TBoolean
}

case class UNTAC_Var(id : Identifier) extends UNTAC_Op{
  override def toString(): String = id.value
  override def getType() : Type = id.getType
}

case class UNTAC_Temp(name:String,tpe:Type) extends UNTAC_Op{
  override def toString(): String = name
  override def getType() : Type = tpe
}

case class UNTAC_Lit(lit_node: Literal) extends UNTAC_Op{
  override def toString(): String = lit_node.toString()
  override def getType() : Type = lit_node match{
    case IntLit(_) => TInt
    case StringLit(_) => TString
    case True() => TBoolean
    case False() => TBoolean
    case t@This() => t.getType
  }
}

case class UNTAC_Comment(comment: String) extends UNTAC_Instr{
  override def toString(): String = comment
  override def getType = TUntyped
}

sealed trait UNTAC_DefTree
case class UNTAC_Program(main: UNTAC_MainObject, classes: List[UNTAC_ClassDecl]) extends UNTAC_DefTree
case class UNTAC_ClassDecl(cdecl : ClassDecl, methods: List[UNTAC_MethodDecl]) extends UNTAC_DefTree
case class UNTAC_MethodDecl(meth : MethodDecl,var tacList : List[UNTAC_Instr],temps:List[UNTAC_Temp]) extends UNTAC_DefTree
case class UNTAC_MainObject(main: MainObject, stats: List[UNTAC_Instr],temps:List[UNTAC_Temp]) extends UNTAC_DefTree