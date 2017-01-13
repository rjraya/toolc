package toolc.abstract_inter.cfg

import scala.collection.mutable.HashSet
import toolc.abstract_inter.tac._

//Only a single instruction per block.
//Each block has an id number, a single instruction and a set of successor and predecessor blocks.
class BasicBlock(val id : Int, val instr : TAC_Instr){
  private val successors = new HashSet[BasicBlock]()
  private val predecessors = new HashSet[BasicBlock]()
  def getPredecessors() = predecessors.toList
  def getSuccessors() = successors.toList
  def addEdge(successor : BasicBlock) = {
    successors.add(successor) //Add successor to this block
    successor.predecessors.add(this) //Add this block as predecessor of successor
  }
}