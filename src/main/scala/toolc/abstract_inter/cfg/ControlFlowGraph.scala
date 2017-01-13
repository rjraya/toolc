package toolc.abstract_inter.cfg

import scala.collection._
import toolc.abstract_inter.tac._

//Manages the collection of Basic Blocks
//Initialize enter and exit blocks with no instruction.
//Iterate over blocks with for (BasicBlock b :  cfg)
class ControlFlowGraph extends Iterable[BasicBlock] {
  private val blocks = new mutable.MutableList[BasicBlock]()
  private var enter : BasicBlock = _
  private var exit : BasicBlock = _

  def addBasicBlock(instr : TAC_Instr) : BasicBlock = {
    blocks += new BasicBlock(blocks.size, instr)
    blocks.last
  }
  //Returns the block for an instruction if it exists. Else it throws an exception.
  def getBlock(i : TAC_Instr) : BasicBlock = blocks.find(b => b.instr.index == i.index).get

  def getEnter = enter
  def getExit = exit
  def setEnter(enter : BasicBlock) = this.enter = enter
  def setExit(exit : BasicBlock) = this.exit = exit
  def iterator() = blocks.iterator //Iterator for blocks in the order they were allocated

  override def toString = blocks.mkString("\n")
}