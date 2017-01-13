package toolc.abstract_inter.dfa

import toolc.abstract_inter.cfg.ControlFlowGraph
import toolc.abstract_inter.tac._

//Entries are pairs of Int, TAC_Op where Int represents the defining instruction and TAC_Op represents that variable which is defined.
class ReachingDefinitionsAnalysis(override val cfg : ControlFlowGraph) extends DataFlowAnalysis[Set[(Int,TAC_Op)]](cfg)  {
  def isForward() = true
  def transfer(instr : TAC_Instr, in : Set[(Int,TAC_Op)]) = genkill(instr) | in.filter{case (i,op) => !((genkill(instr) map{x => x._2}) contains op)}
  def boundary() = Set[(Int,TAC_Op)]()
  def meet(t1 : Set[(Int,TAC_Op)], t2: Set[(Int,TAC_Op)]): Set[(Int,TAC_Op)] = t1 | t2
  def top() = Set[(Int,TAC_Op)]()
  def equals(t1 : Set[(Int,TAC_Op)], t2: Set[(Int,TAC_Op)]): Boolean = t1.equals(t2)

  def addIfVariable(in: Set[(Int,TAC_Op)]): Set[(Int,TAC_Op)] = in.filter(x => x._2.isInstanceOf[TAC_Var] || x._2.isInstanceOf[TAC_Temp])

  def genkill(instr: TAC_Instr):Set[(Int,TAC_Op)] = {
    instr match {
      case TAC_ArrayLength(lhs,_,index) => Set((index,lhs))
      case TAC_NewClass(lhs,_,index) => addIfVariable(Set((index,lhs)))
      case TAC_NewIntArray(lhs,_,index) => addIfVariable(Set((index,lhs)))
      case TAC_MethodCall(lhs,_,_,_,index) => addIfVariable(Set((index,lhs)))
      case TAC_Assign(lhs,_,index) => addIfVariable(Set((index,lhs)))
      case TAC_ArrayAssign(array, arrindex, value,index ) => Set()
      case TAC_ArrayRead(lhs: TAC_Op,_,_,index) => addIfVariable(Set((index,lhs)))
      case TAC_BinOp(lhs: TAC_Op,_, _,_,index : Int) => addIfVariable(Set((index,lhs)))
      case TAC_UnOp(lhs: TAC_Op, _,_,index) => addIfVariable(Set((index,lhs)))
      case _ => Set()
    }
  }
}