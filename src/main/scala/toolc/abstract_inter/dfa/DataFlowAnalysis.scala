package toolc.abstract_inter.dfa

import toolc.abstract_inter.cfg._
import toolc.abstract_inter.tac._
import scala.collection._

//Type T: the type of value contained in the lattice.
//Enter and exit blocks contain "NoOp" TAC instruction. No transfer function applied.
//In forward analysis, set in[enter] to Top and in backward analysis out[exit] to Top for simplicity.
abstract class DataFlowAnalysis[T](val cfg: ControlFlowGraph) {
  def isForward : Boolean
  def boundary(): T
  def top(): T
  def meet(t1: T, t2: T): T
  def equals(t1: T, t2: T): Boolean
  def transfer(instr: TAC_Instr, t: T): T

  def solve() = {
    if (isForward) {
      outs.put(cfg.getEnter, boundary())
      val outChanges = (b: BasicBlock) => {
        if (b == cfg.getEnter) false
        else {
          val oldOutValue = out(b)
          val predecessorOuts = b.getPredecessors().map(out)
          val newInValue =  predecessorOuts.foldLeft(top())(meet)
          ins.put(b, newInValue)
          val newOutValue = transfer(b.instr, newInValue)
          outs.put(b, newOutValue)
          !equals(oldOutValue,newOutValue)
        }
      }
      while (cfg.map(outChanges).reduce(_ || _)){}
    } else {
      ins.put(cfg.getExit, boundary())
      val inChanges = (b: BasicBlock) => {
        if (b == cfg.getExit) false
        else {
          val oldInValue = in(b)
          val successorIns = b.getSuccessors().map(in)
          val newOutValue = successorIns.foldLeft(top())(meet)
          outs.put(b,newOutValue)
          val newInValue = transfer(b.instr,newOutValue)
          ins.put(b, newInValue)
          !equals(oldInValue,newInValue)
        }
      }
      while (cfg.map(inChanges).reduce(_ || _)){}
    }
  }

  val ins: mutable.HashMap[BasicBlock, T] = new mutable.HashMap()
  val outs: mutable.HashMap[BasicBlock, T] = new mutable.HashMap()
  def in(b: BasicBlock): T = ins.getOrElse(b, top())
  def out(b: BasicBlock): T = outs.getOrElse(b, top())

  //Print in/out values of each basic block.
  override def toString : String = {
    cfg.foldLeft("")(
      (s,b) => s + "Block " + b.id + "\n  " +
        "IN:  " + in(b) + "\n " + b.instr + "\n  " +
        "OUT: " + out(b) + "\n\n"
    )
  }
}