package toolc.abstract_inter.opt

import toolc.abstract_inter.cfg._
import toolc.abstract_inter.dfa._
import toolc.abstract_inter.tac._
import toolc.ast.Trees._

class DeadCodeElimination extends Optimization {
  def optimize(m : TAC_MethodDecl) : List[TAC_Instr]= {
    val generator = new CFG_Generator()
    val cfg : ControlFlowGraph = generator.makeCFG(m)
    val lives = new LiveVariableAnalysis(cfg)
    lives.solve()
    val tacInstructions = m.tacList
    // Map each instruction to either the original instruction or a comment if not reachable.
    val optimized = tacInstructions map { instr =>
      instr match {
        case TAC_ArrayLength(lhs, _, _) => removeIfNotLive(lhs,instr,lives,cfg,m)
        case TAC_NewClass(lhs, _, _) => removeIfNotLive(lhs,instr,lives,cfg,m)
        case TAC_NewIntArray(lhs, _, _) => removeIfNotLive(lhs,instr,lives,cfg,m)
        case TAC_MethodCall(lhs, _, _, _, _) => removeIfNotLive(lhs,instr,lives,cfg,m)
        case TAC_Assign(lhs, rhs, index: Int) => removeIfNotLive(lhs,instr,lives,cfg,m)
        case TAC_ArrayAssign(array, _, _, _) => removeIfNotLive(array, instr, lives, cfg,m)
        case TAC_ArrayRead(lhs, _, _, _) => removeIfNotLive(lhs,instr,lives,cfg,m)
        case TAC_BinOp(lhs, _, _, _, _) => removeIfNotLive(lhs,instr,lives,cfg,m)
        case TAC_UnOp(lhs, _, _, _) => removeIfNotLive(lhs,instr,lives,cfg,m)
        case _ => instr
      }
    }
    optimized
  }

  //Limitations of intraprocedural analysis.
  //Class variable computation cannot be removed because it maybe reused in other computation.
  //Method calls cannot be removed because they may change the class variables and thus the program behaviour.
  def removeIfNotLive(lhs: TAC_Op,instr: TAC_Instr, lives: LiveVariableAnalysis, cfg: ControlFlowGraph,m:TAC_MethodDecl): TAC_Instr = {
    val intraproceduralRestrictions = isClassMember(lhs,m) || instr.isInstanceOf[TAC_MethodCall]
    if(lives.out(cfg.getBlock(instr)).contains(lhs) || intraproceduralRestrictions) instr
    else TAC_Comment("Removed dead code: " + instr,instr.index)

  }
  def isClassMember(lhs:TAC_Op,m:TAC_MethodDecl): Boolean = lhs match{
    case TAC_Var(id : Identifier) =>
      !m.meth.args.map{case Formal(tpe,i) => i}.contains(id) && !m.meth.vars.map{case VarDecl(tpe,i) => i}.contains(id)
    case _ => false
  }

}
