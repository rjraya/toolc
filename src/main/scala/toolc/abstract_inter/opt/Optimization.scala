package toolc.abstract_inter.opt

import toolc.abstract_inter.tac._
import toolc.ast.Trees._

abstract class Optimization {
  //Apply the optimization to method m
  def optimize(m : TAC_MethodDecl) : List[TAC_Instr]
}
