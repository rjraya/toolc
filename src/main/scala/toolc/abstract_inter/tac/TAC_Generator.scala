package toolc.abstract_inter.tac

import toolc.ast.Trees._
import toolc.analyzer.Types._

class TAC_Generator {
  var labelCount = 0

  def gen(p:Program): TAC_Program = {
    var tacClasses = List[TAC_ClassDecl]()
    for(c <- p.classes){
      var tacMethods = List[TAC_MethodDecl]()
      for(m <- c.methods){
        val tacList = gen(m)
        tacMethods = tacMethods :+ TAC_MethodDecl(m,tacList.list,tacList.numTempVariables)
      }
      tacClasses = tacClasses :+ TAC_ClassDecl(c,tacMethods)
    }
    val tacList = gen(p.main)
    val tacMain = TAC_MainObject(p.main,tacList.list,tacList.numTempVariables)
    TAC_Program(tacMain,tacClasses)
  }

  def gen(main : MainObject): TAC_List ={
    val tacInstr = new TAC_List()
    for (stat <- main.stats) gen(stat, tacInstr)
    tacInstr
  }

  def gen(m: MethodDecl) : TAC_List = {
    val tacInstr = new TAC_List()
    for (stat <- m.stats) gen(stat, tacInstr)
    tacInstr.add(TAC_Ret(gen(m.retExpr, tacInstr),tacInstr.size()))
    tacInstr
  }

  def gen(stmt: StatTree, tacList: TAC_List): Unit = {
    stmt match {
      case Block(stats: List[StatTree]) => for (s <- stats) gen(s, tacList)
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        val elseLabel = TAC_Label("L" + labelCount,-1)
        labelCount += 1
        val endLabel = TAC_Label("L" + labelCount,-1)
        labelCount += 1
        val temp = tacList.alloc(TBoolean)

        val cond = gen(expr, tacList)
        tacList.add(TAC_UnOp(temp, cond, "!",tacList.size()))
        tacList.add(TAC_CJump(elseLabel, temp,tacList.size()))
        gen(thn, tacList)
        els match {
          case Some(elseStat) =>
            tacList.add(TAC_Jump(endLabel,tacList.size()))
            tacList.add(elseLabel)
            gen(elseStat, tacList)
            endLabel.index = tacList.size()
            tacList.add(endLabel)
          case None =>
            elseLabel.index = tacList.size()
            tacList.add(elseLabel)
        }
      case While(expr: ExprTree, stat: StatTree) =>
        val topLabel: TAC_Label = TAC_Label("L" + labelCount,tacList.size())
        labelCount += 1
        val endLabel: TAC_Label = TAC_Label("L" + labelCount,-1)
        labelCount += 1
        val temp = tacList.alloc(TBoolean)

        tacList.add(topLabel)
        val cond = gen(expr, tacList)
        tacList.add(TAC_UnOp(temp, cond, "!",tacList.size()))
        tacList.add(TAC_CJump(endLabel, temp,tacList.size()))
        gen(stat, tacList)
        tacList.add(TAC_Jump(topLabel,tacList.size()))
        endLabel.index = tacList.size()
        tacList.add(endLabel)
      case Assign(id: Identifier, expr: ExprTree) =>
        val temp = gen(expr,tacList)
        val v = TAC_Var(id)
        tacList.add(TAC_Assign(v, temp,tacList.size()))
      case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
        val temp = gen(expr,tacList)
        val arr = TAC_Var(id)
        val ind = gen(index,tacList)
        tacList.add(TAC_ArrayAssign(arr,ind,temp,tacList.size()))
      case DoExpr(expr: ExprTree) => tacList.add(TAC_DoExpr(gen(expr, tacList),tacList.size()))
      case Println(expr: ExprTree) => tacList.add(TAC_PrintLn(gen(expr, tacList),tacList.size()))
    }
  }

  def gen(expr: ExprTree, tacList: TAC_List): TAC_Op = {
    expr match {
      case And(lhs, rhs) => makeBinop(tacList,lhs,rhs,"&&")
      case Or(lhs: ExprTree, rhs: ExprTree) => makeBinop(tacList,lhs,rhs,"||")
      case Not(expr: ExprTree) =>
        val temp = tacList.alloc(TBoolean)
        val tacExpr = gen(expr, tacList)
        tacList.add(TAC_UnOp(temp, tacExpr, "!",tacList.size()))
        temp
      case Plus(lhs,rhs) => makeBinop(tacList, lhs, rhs, "+")
      case Minus(lhs,rhs) => makeBinop(tacList, lhs, rhs, "-")
      case Times(lhs,rhs) => makeBinop(tacList, lhs, rhs, "*")
      case Div(lhs,rhs) => makeBinop(tacList, lhs, rhs, "/")
      case LessThan(lhs,rhs) => makeBinop(tacList, lhs, rhs,  "<")
      case Equals(lhs,rhs) => makeBinop(tacList, lhs, rhs,  "==")
      case ar@ArrayRead(array: ExprTree, index: ExprTree) =>
        val temp = tacList.alloc(ar.getType)
        val arr = gen(array, tacList)
        val ind = gen(index, tacList)
        tacList.add(TAC_ArrayRead(temp, arr, ind,tacList.size()))
        temp
      case al@ArrayLength(array: ExprTree) =>
        val temp2 = gen(array, tacList)
        val temp = tacList.alloc(al.getType)
        tacList.add(TAC_ArrayLength(temp, temp2,tacList.size()))
        temp
      case nia@NewIntArray(size: ExprTree) =>
        val arrsize = gen(size, tacList)
        val temp = tacList.alloc(nia.getType)
        tacList.add(TAC_NewIntArray(temp, arrsize,tacList.size()))
        temp
      case New(tpe: Identifier) =>
        val temp = tacList.alloc(tpe.getType)
        tacList.add(TAC_NewClass(temp,tpe,tacList.size()))
        temp
      case mc@MethodCall(obj,meth,args) =>
        val paramList = makeParams(args, tacList)
        val receiver = gen(obj, tacList)
        val temp = tacList.alloc(mc.getType)
        tacList.add(TAC_MethodCall(temp, receiver, meth,paramList,tacList.size()))
        temp
      case il@IntLit(value) => TAC_Lit(il,il.getType)
      case tl@True() => TAC_Lit(tl,tl.getType)
      case fl@False() => TAC_Lit(fl,fl.getType)
      case sl@StringLit(value) => TAC_Lit(sl,sl.getType) //maybe different
      case thl@This() => TAC_Lit(thl,thl.getType)
      case Variable(id: Identifier) => TAC_Var(id)
    }
  }


  def makeBinop(tacList: TAC_List, lhs: ExprTree, rhs: ExprTree, binop: String): TAC_Op = {
    val tpe = binop match{
      case "-"|"*"|"/" => TInt
      case "+" => if (lhs.getType == TString || rhs.getType == TString) TString else TInt
      case "=="|"<"|"&&"|"||" => TBoolean
    }
    val temp = tacList.alloc(tpe)
    val left = gen(lhs, tacList)
    val right = gen(rhs, tacList)
    tacList.add(TAC_BinOp(temp, left, right, binop,tacList.size()))
    temp
  }

  def makeParams(params: List[ExprTree], tacList: TAC_List): List[TAC_Op] = params.map(gen(_, tacList))
}
