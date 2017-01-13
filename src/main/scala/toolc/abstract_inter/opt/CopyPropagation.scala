package toolc.abstract_inter.opt

import toolc.abstract_inter.dfa.ReachingCopiesAnalysis
import toolc.abstract_inter.cfg._
import toolc.abstract_inter.tac._

class CopyPropagation extends Optimization {

  def optimize(m : TAC_MethodDecl) = {
    val cfg : ControlFlowGraph = new CFG_Generator().makeCFG(m)
    val copies = new ReachingCopiesAnalysis(cfg)
    copies.solve()

    val optimized = m.tacList map {instr => {
        val definitions = copies.in(cfg.getBlock(instr))
        instr match {
          case TAC_CJump(label,condition: TAC_Op,i) => TAC_CJump(label, getOp(condition,definitions),i)
          case TAC_Ret(value: TAC_Op,i) => TAC_Ret(getOp(value,definitions),i)
          case TAC_ArrayLength(lhs,array: TAC_Op,i) => TAC_ArrayLength(lhs,getOp(array,definitions),i)
          case TAC_MethodCall(lhs,receiver: TAC_Op,meth,paramList: List[TAC_Op],i) =>
            TAC_MethodCall(lhs,getOp(receiver,definitions),meth,paramList.map(getOp(_,definitions)),i)
          case TAC_PrintLn(param: TAC_Op,i) => TAC_PrintLn(getOp(param,definitions),i)
          case TAC_DoExpr(param: TAC_Op ,i) => TAC_DoExpr(getOp(param,definitions),i)
          case TAC_Assign(lhs,rhs:TAC_Op,i) => TAC_Assign(lhs,getOp(rhs,definitions),i)
          case TAC_ArrayAssign(array,index: TAC_Op,value: TAC_Op,i) =>
            TAC_ArrayAssign(array, getOp(index,definitions), getOp(value,definitions),i)
          case TAC_ArrayRead(lhs,array,index: TAC_Op,i) => TAC_ArrayRead(lhs,array,getOp(index,definitions),i)
          case TAC_BinOp(lhs,lexpr: TAC_Op,rexpr: TAC_Op,binop,i) => TAC_BinOp(lhs,getOp(lexpr,definitions),getOp(rexpr,definitions),binop,i)
          case TAC_UnOp(lhs,expr: TAC_Op,unop,i) => TAC_UnOp(lhs, getOp(expr,definitions), unop,i)
          case _ => instr
        }
      }
    }

    optimized
  }
  def getOp(op: TAC_Op, m: Map[TAC_Op, TAC_Op]) : TAC_Op = m.getOrElse(op, op)
}
