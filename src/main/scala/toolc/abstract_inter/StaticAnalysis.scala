package toolc.abstract_inter

import toolc.abstract_inter.tac._
import toolc.abstract_inter.opt._
import toolc.ast.Trees.Program
import toolc.utils.{Context, Pipeline}
import toolc.abstract_inter.untac._

object StaticAnalysis extends Pipeline[Program, UNTAC_Program] {

  def filterComments(l:List[TAC_Instr]) = l.filter{i => !i.isInstanceOf[TAC_Comment]}

  def run(ctx: Context)(prog: Program): UNTAC_Program = {
    val tacProgram = new TAC_Generator().gen(prog)
    //perform live variable analysis on each method
    for (c <- tacProgram.classes) {
      for (m <- c.methods) {
        var optDeadCode = List[TAC_Instr]()
        optDeadCode = (new DeadCodeElimination).optimize(m)
        m.tacList = optDeadCode
      }
    }

    val untacProgram = new UNTAC_Generator().gen(tacProgram)

    for(c <- untacProgram.classes){
      for(m <- c.methods){
        println("---------------------------")
        println(m.meth.id.value)
        println("---------------------------")
        for(instr <- m.tacList){
          println(instr)
        }
      }
    }

    untacProgram
  }
}

