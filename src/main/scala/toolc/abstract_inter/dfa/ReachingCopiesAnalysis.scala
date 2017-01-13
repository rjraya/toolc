package toolc.abstract_inter.dfa

import toolc.abstract_inter.cfg.ControlFlowGraph
import toolc.abstract_inter.tac._

//The idea behind the copy-propagation tranformation is to use v for u wherever possible after te copy statement u = v
class ReachingCopiesAnalysis(override val cfg : ControlFlowGraph) extends DataFlowAnalysis[Map[TAC_Op,TAC_Op]](cfg)  {
  def boundary() = Map[TAC_Op,TAC_Op]()
  def top() = null
  def equals(t1 : Map[TAC_Op,TAC_Op], t2: Map[TAC_Op,TAC_Op]) = (t1,t2) match {
    case (null,null) => true
    case (null,_) | (_,null) => false
    case _ => t1.equals(t2)
  }
  def isForward = true
  def meet(t1 : Map[TAC_Op,TAC_Op], t2: Map[TAC_Op,TAC_Op]) = (t1,t2) match {
    case (null,_) => t2
    case (_,null) => t1
    case _ => t1.filter{case (k,v) => t2.get(k) == v} //intersection
  }

  def transfer(instr : TAC_Instr, in: Map[TAC_Op,TAC_Op]): Map[TAC_Op,TAC_Op] = {
    instr match{
      case TAC_Assign(lhs,rhs: TAC_Op,_) => updateCopies(lhs,in) + (lhs -> rhs)
      case TAC_BinOp(lhs,_,_,_,_) => updateCopies(lhs,in)
      case TAC_UnOp(lhs,_,_,_) => updateCopies(lhs,in)
      case TAC_NewClass(lhs,_,_) => updateCopies(lhs,in)
      case TAC_ArrayLength(lhs,_,_) => updateCopies(lhs,in)
      case TAC_MethodCall(lhs,_,_,_,_) => updateCopies(lhs,in)
      case TAC_NewIntArray(lhs,_,_) => updateCopies(lhs,in)
      case TAC_ArrayRead(lhs,_,_,_) => updateCopies(lhs,in)
      case _ => in
    }
  }
  def updateCopies(t:TAC_Op, set: Map[TAC_Op,TAC_Op]) = set.filterNot{case (k,v) => k == t || v == t}
}
