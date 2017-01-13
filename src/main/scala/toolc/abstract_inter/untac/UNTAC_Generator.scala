package toolc.abstract_inter.untac

import toolc.abstract_inter.tac._

//si no encontrases un siguiente valor elimina la instrucción
//siempre hay una única siguiente instrucción
class UNTAC_Generator {

  def gen(tacProgram : TAC_Program) : UNTAC_Program = {
    val (tacMainInstr,tempsMain) = removeTemporalVariables(tacProgram.main.stats)
    val mainUntac = UNTAC_MainObject(tacProgram.main.main,tacMainInstr,tempsMain)
    val untacClasses =
      for(c <- tacProgram.classes) yield{
        val untacMethods =
          for(m <- c.methods) yield {
            val (tacInstr,temps) = removeTemporalVariables(m.tacList)
            UNTAC_MethodDecl(m.meth,tacInstr,temps)
          }
        UNTAC_ClassDecl(c.cdecl,untacMethods)
      }
    UNTAC_Program(mainUntac,untacClasses)
  }

  //For each instruction in the list
  //If this instruction defines a temporal variable
  //Then substitute all the appearences of this temporal variable ahead
  //Else continue with next instruction
  def removeTemporalVariables(tacList : List[TAC_Instr]) : (List[UNTAC_Instr],List[UNTAC_Temp]) = {
    val listOfRemainingTemporals = List[UNTAC_Temp]()
    val untacList = tacList map convert
    var modified : List[UNTAC_Instr] = untacList
    var index = 0
    var continue = true
    while(continue){
      val instr = modified(index)
      modified = instr match {
        //instructions that define temporal variables
        case UNTAC_ArrayLength(Some(lhs),_) => modified.take(index) ++ removeTemp(modified.drop(index+1),lhs,instr);
        case UNTAC_NewClass(Some(lhs),_) => modified.take(index) ++ removeTemp(modified.drop(index+1),lhs,instr)
        case UNTAC_NewIntArray(Some(lhs),_) => modified.take(index) ++ removeTemp(modified.drop(index+1),lhs,instr)
        case UNTAC_MethodCall(Some(lhs),r,meth,paramList) => modified.take(index) ++ removeTemp(modified.drop(index+1),lhs,instr)
          //if you don't find a use of this call, put the call here and remove the result with pop (none receiver)
          //if you find a use of the call, copy there the call. it is guaranteed that there will be only one such call
          if(scanForOccurrence(lhs,modified.drop(index+1))){
            modified.take(index) ++ removeTemp(modified.drop(index+1),lhs,instr)
          }else{
            modified.take(index) ++ List(UNTAC_MethodCall(None,r,meth,paramList))++ modified.drop(index+1)
          }
        case UNTAC_Assign(t,lhs,rhs) => t match {
          case Some(tmp) => modified.take(index) ++ removeTemp(modified.drop(index+1),tmp,instr)
          case None => index += 1; modified
        }
        case UNTAC_ArrayRead(Some(lhs),_,_) => modified.take(index) ++ removeTemp(modified drop(index+1),lhs,instr)
        case UNTAC_BinOp(Some(lhs),_,_,_) => modified.take(index) ++ removeTemp(modified.drop(index+1),lhs,instr)
        case UNTAC_UnOp(Some(lhs),_,_) => modified.take(index) ++ removeTemp(modified.drop(index+1),lhs,instr)
        case _ => index += 1; modified
      }
      continue = index < modified.size
    }
    (modified,listOfRemainingTemporals)
  }

  //With properties on optimizations that ensure that temporal variables are defined only once and reappear only once
  //this method could look only for a certain number of instructions ahead.
  //Important remarks:
  //When copying ahead and find an assigment substitute the copying value for that assignment lhs
  def removeTemp(untacList : List[UNTAC_Instr],tmp : UNTAC_Temp, value : UNTAC_Instr): List[UNTAC_Instr] = {
    var notFound = true
    var notEnded = true
    var i = 0
    var untacListLocal = untacList
    var localValue = value

    while(notEnded){
      val instr = untacListLocal(i)
      var newInstr = instr
      notFound = false
      newInstr = instr match{
        case UNTAC_CJump(l,UNTAC_Temp(name,_)) if name == tmp.name=> UNTAC_CJump(UNTAC_Label(l.label),localValue)
        case UNTAC_Ret(UNTAC_Temp(name,_)) if name == tmp.name => UNTAC_Ret(localValue)
        case UNTAC_ArrayLength(t,UNTAC_Temp(name,_)) if name == tmp.name => UNTAC_ArrayLength(t,localValue)
        case UNTAC_NewIntArray(t,UNTAC_Temp(name,_)) if name == tmp.name => UNTAC_NewIntArray(t,localValue)
        case UNTAC_MethodCall(t,UNTAC_Temp(name,_),id,paramList) if name == tmp.name => UNTAC_MethodCall(t,value,id,removeTempFromList(paramList,tmp,localValue))
        case UNTAC_MethodCall(t,receiver,id,paramList) => UNTAC_MethodCall(t,receiver,id,removeTempFromList(paramList,tmp,localValue))
        case UNTAC_PrintLn(UNTAC_Temp(name,_)) if name == tmp.name => UNTAC_PrintLn(localValue)
        case UNTAC_DoExpr(UNTAC_Temp(name,_)) if name == tmp.name => UNTAC_DoExpr(localValue)
        case UNTAC_Assign(t,l,UNTAC_Temp(name,_)) if name == tmp.name =>
          val preservedLocalValue = localValue
          (t,l) match{
              case (Some(tmp),_) => localValue = tmp
              case (_,Some(lhs)) => localValue = lhs
          }
          UNTAC_Assign(t,l,preservedLocalValue)
        case UNTAC_ArrayAssign(array,arrindex,avalue) =>
          val res = removeTempFromList(List(arrindex,avalue),tmp,localValue)
          UNTAC_ArrayAssign(array,res.head,res(1))
        case UNTAC_ArrayRead(t,array,arrindex) =>
          val res = removeTempFromList(List(array,arrindex),tmp,localValue)
          UNTAC_ArrayRead(t,res.head,res(1))
        case UNTAC_BinOp(t,lexpr,rexpr,b) =>
          val res = removeTempFromList(List(lexpr,rexpr),tmp,localValue)
          UNTAC_BinOp(t,res.head,res(1),b)
        case UNTAC_UnOp(t,UNTAC_Temp(name,_),u) if name == tmp.name =>  UNTAC_UnOp(t,localValue,u)
        case _ => notFound = true; instr
      }
      if(notFound == false) untacListLocal = untacListLocal.updated(i,newInstr)
      i += 1
      notEnded = i < untacList.size
    }
    untacListLocal
  }

  def removeTempFromList(paramList : List[UNTAC_Instr],tmp : UNTAC_Temp,value:UNTAC_Instr) : List[UNTAC_Instr] = {
    paramList map {param =>
      param match{
        case UNTAC_Temp(name,_) if name == tmp.name => value
        case _ => param
      }
    }
  }

  def convert(instr:TAC_Instr):UNTAC_Instr = instr match{
    case TAC_Label(label,_) => UNTAC_Label(label)
    case TAC_Jump(l,_) => UNTAC_Jump(UNTAC_Label(l.label))
    case TAC_CJump(l,cond,_) => UNTAC_CJump(UNTAC_Label(l.label),convertOp(cond))
    case TAC_Ret(value,_) => UNTAC_Ret(convertOp(value))
    case TAC_ArrayLength(t,array,_) => UNTAC_ArrayLength(Some(UNTAC_Temp(t.name,t.tpe)),convertOp(array))
    case TAC_NewClass(t,id,_) => UNTAC_NewClass(Some(UNTAC_Temp(t.name,t.tpe)),id)
    case TAC_NewIntArray(t,size,_) => UNTAC_NewIntArray(Some(UNTAC_Temp(t.name,t.tpe)),convertOp(size))
    case TAC_MethodCall(t,receiver: TAC_Op,meth, paramList: List[TAC_Op],_) =>
      UNTAC_MethodCall(Some(UNTAC_Temp(t.name,t.tpe)),convertOp(receiver),meth,paramList map convertOp)
    case TAC_PrintLn(param,_) => UNTAC_PrintLn(convertOp(param))
    case TAC_DoExpr(param,_) => UNTAC_DoExpr(convertOp(param))
    case TAC_Assign(lhs,rhs,_) => lhs match{
      case t@TAC_Temp(_,_) => UNTAC_Assign(Some(UNTAC_Temp(t.name,t.tpe)),None,convertOp(rhs))
      case v@TAC_Var(_) => UNTAC_Assign(None,Some(UNTAC_Var(v.id)),convertOp(rhs))
    }
    case TAC_ArrayAssign(array,arrindex,value,_) => UNTAC_ArrayAssign(UNTAC_Var(array.id),convertOp(arrindex),convertOp(value))
    case TAC_ArrayRead(t,array,arrindex,_) => UNTAC_ArrayRead(Some(UNTAC_Temp(t.name,t.tpe)),convertOp(array),convertOp(arrindex))
    case TAC_BinOp(t,lexpr,rexpr,binop,_) => UNTAC_BinOp(Some(UNTAC_Temp(t.name,t.tpe)),convertOp(lexpr),convertOp(rexpr),binop)
    case TAC_UnOp(t,rhs,unop,_) => UNTAC_UnOp(Some(UNTAC_Temp(t.name,t.tpe)),convertOp(rhs),unop)
    case TAC_Comment(comment,_) => UNTAC_Comment(comment)
  }

  def convertOp(op:TAC_Op):UNTAC_Op = op match{
    case TAC_Var(id) => UNTAC_Var(id)
    case TAC_Lit(lit_node,tpe) => UNTAC_Lit(lit_node)
    case TAC_Temp(name,tpe) => UNTAC_Temp(name,tpe)
  }
  //Tells if a temporal variables occurs in a list of instructions
  def scanForOccurrence(tmp:UNTAC_Temp,untacList : List[UNTAC_Instr]) : Boolean = {
    var notEnded = true
    var notFound = true
    var i = 0

    while(notEnded && notFound){
      val instr = untacList(i)
      instr match{
        case UNTAC_CJump(l,UNTAC_Temp(name,_)) if name == tmp.name => notFound = false
        case UNTAC_Ret(UNTAC_Temp(name,_)) if name == tmp.name => notFound = false
        case UNTAC_ArrayLength(t,UNTAC_Temp(name,_)) if name == tmp.name => notFound = false
        case UNTAC_NewIntArray(t,UNTAC_Temp(name,_)) if name == tmp.name => notFound = false
        case UNTAC_MethodCall(t,UNTAC_Temp(name,_),id,paramList) if name == tmp.name => notFound = false
        case UNTAC_MethodCall(t,receiver,id,paramList) => notFound = !(paramList map{x => scanForOccurrence(tmp,List(x))}).foldLeft(false)(_ || _)
        case UNTAC_PrintLn(UNTAC_Temp(name,_)) if name == tmp.name => notFound = false
        case UNTAC_DoExpr(UNTAC_Temp(name,_)) if name == tmp.name => notFound = false
        case UNTAC_Assign(t,l,UNTAC_Temp(name,_)) if name == tmp.name => notFound = false
        case UNTAC_ArrayAssign(array,arrindex,avalue) =>
          notFound = !(scanForOccurrence(tmp,List(array)) || scanForOccurrence(tmp,List(arrindex)) || scanForOccurrence(tmp,List(avalue)))
        case UNTAC_ArrayRead(t,array,arrindex) =>
          notFound = !(scanForOccurrence(tmp,List(array)) || scanForOccurrence(tmp,List(arrindex)))
        case UNTAC_BinOp(t,lexpr,rexpr,b) => notFound = !(scanForOccurrence(tmp,List(lexpr)) || scanForOccurrence(tmp,List(rexpr)))
        case UNTAC_UnOp(t,UNTAC_Temp(name,_),u) if name == tmp.name =>  notFound = false
        case UNTAC_Temp(name,tpe) if name == tmp.name => notFound = false
        case _ =>
      }
      i += 1
      notEnded = i < untacList.size
    }
    !notFound
  }

}
