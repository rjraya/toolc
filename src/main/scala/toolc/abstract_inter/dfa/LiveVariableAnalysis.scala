package toolc.abstract_inter.dfa

import toolc.abstract_inter.cfg.ControlFlowGraph
import toolc.abstract_inter.tac._

class LiveVariableAnalysis(override val cfg : ControlFlowGraph) extends DataFlowAnalysis[Set[TAC_Op]](cfg)  {
  def isForward = false
  def transfer(instr :TAC_Instr, out: Set[TAC_Op]) = use(instr) | (out -- define(instr))
  def boundary() = Set[TAC_Op]()
  def meet(t1 : Set[TAC_Op], t2: Set[TAC_Op]) = t1 | t2
  def top() = Set[TAC_Op]()
  def equals(t1 : Set[TAC_Op], t2: Set[TAC_Op]) = t1.equals(t2)

  def use(instr:TAC_Instr): Set[TAC_Op] = {
    instr match {
      case TAC_CJump(_, condition,_) => addIfVariable(Set(condition))
      case TAC_ArrayLength(_, array,_) => addIfVariable(Set(array))
      case TAC_NewIntArray(_,size,_) => addIfVariable(Set(size))
      case TAC_MethodCall(_, receiver,_, paramList,_) => addIfVariable(paramList.toSet + receiver)
      case TAC_Ret(value,_) => addIfVariable(Set(value))
      case TAC_PrintLn(param,_) => addIfVariable(Set(param))
      case TAC_DoExpr(param,_) => addIfVariable(Set(param))
      case TAC_Assign(_,rhs,_) => addIfVariable(Set(rhs))
      case TAC_ArrayAssign(array,index, value,_) => addIfVariable(Set(array, index, value))
      case TAC_ArrayRead(_,array,index,_) => addIfVariable(Set(array, index))
      case TAC_BinOp(_,lExpr,rExpr,_,_) => addIfVariable(Set(lExpr, rExpr))
      case TAC_UnOp(_, expr,_,_) => addIfVariable(Set(expr))
      case _ => Set()
    }
  }

  //Make sure the variables added to "defined" were not used on the same instruction
  def define(instr:TAC_Instr): Set[TAC_Op] = {
    instr match {
      case TAC_NewClass(lhs,_,_) => addIfVariable(Set(lhs)) -- use(instr)
      case TAC_NewIntArray(lhs,_,_)  => addIfVariable(Set(lhs)) -- use(instr)
      case TAC_ArrayLength(lhs,_,_) => addIfVariable(Set(lhs)) -- use(instr)
      case TAC_MethodCall(lhs,_,_,_,_) => addIfVariable(Set(lhs)) -- use(instr)
      case TAC_Assign(lhs,_,_) => addIfVariable(Set(lhs)) -- use(instr)
      case TAC_ArrayRead(lhs,_,_,_)  => addIfVariable(Set(lhs)) -- use(instr)
      case TAC_BinOp(lhs,_,_,_,_) => addIfVariable(Set(lhs)) -- use(instr)
      case TAC_UnOp(lhs,_,_,_) => addIfVariable(Set(lhs)) -- use(instr)
      case _ => Set()
    }
  }

  //only adds the variable to the set if it is a temp or a variable
  def addIfVariable(in: Set[TAC_Op]): Set[TAC_Op] = in.filter(x => x.isInstanceOf[TAC_Var] || x.isInstanceOf[TAC_Temp])
}