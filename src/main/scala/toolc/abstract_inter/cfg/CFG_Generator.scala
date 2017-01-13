package toolc.abstract_inter.cfg
import toolc.abstract_inter.tac._
//Here we implement intraprocedural optimization.
class CFG_Generator {
  def makeCFG(method: TAC_MethodDecl): ControlFlowGraph = {
    val cfg = new ControlFlowGraph
    val tacList = method.tacList

    cfg.setEnter(cfg.addBasicBlock(TAC_Comment("Entry",-1)))
    cfg.setExit(cfg.addBasicBlock(TAC_Comment("Exit",-1)))
    for(instr <- tacList) cfg.addBasicBlock(instr)

    var previous = cfg.getEnter
    var current  = cfg.getEnter
    //Add in edges for jumps and return
    for(instr <- tacList){
      current = cfg.getBlock(instr)
      //previous--->current
      previous.instr match {
        case TAC_Jump(label,_) =>
        case _ => previous.addEdge(current)
      }
      //current--->previous
      instr match {
        case TAC_Jump(label,_) => current.addEdge(cfg.getBlock(label))
        case TAC_CJump(label,_,_) => current.addEdge(cfg.getBlock(label))
        case _ =>
      }
      previous = current
    }
    current.addEdge(cfg.getExit) //add edge from ret to exit
    cfg
  }
}
