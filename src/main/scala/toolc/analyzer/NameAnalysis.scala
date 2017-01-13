package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectSymbols(prog: Program): GlobalScope = {

      val global = new GlobalScope

      //Collect main class symbol
      val mcSym = new MainSymbol(prog.main.id.value)
      global.mainClass = mcSym
      prog.main.setSymbol(mcSym)
      prog.main.id.setSymbol(mcSym)

      if(mcSym.name == "Object") error("Main object named Object at " + prog.main.id.position,mcSym)

      //Collect class symbols
      for (c <- prog.classes) {
        val cs: ClassSymbol = new ClassSymbol(c.id.value)
        cs.setPos(c); c.setSymbol(cs); c.id.setSymbol(cs)

        if(cs.name == mcSym.name)  error("Class named as main object at " + c.id.position,cs)
        if(cs.name == "Object") error("Class named Object at " + c.id.position,cs)

        global.lookupClass(cs.name) match {
          case None => global.classes += (cs.name -> cs)
          case _ => error("Redefinition of class " + cs.name + " at " + c.id.position,cs)
        }

      }

      // Set parent Symbols
      for {
        cls <- prog.classes
        clSym = global.classes(cls.id.value)
        par <- cls.parent
      } yield {
        global.lookupClass(par.value) match {
          case None =>
            error(s"Class ${clSym.name} extends class ${par.value} which is not defined.", par)
          case Some(parSym) =>
            clSym.parent = Some(parSym)
            par.setSymbol(parSym)
        }
      }

      // Check there are no cycles in the inheritance graph
      prog.classes foreach { cls =>
        val clsSym = cls.getSymbol

        def mkChain(curr: ClassSymbol): List[ClassSymbol] = {
          curr.parent match {
            case None => List(curr)
            case Some(`clsSym`) => List(curr, clsSym)
            case Some(p) => curr :: mkChain(p)
          }
        }

        val chain = mkChain(clsSym)

        if (chain.size > 1 && chain.head == chain.last) {
          fatal("Cyclic inheritance: " + chain.map(_.name).mkString(" -> "))
        }
      }

      // We now know that every class is unique and the inheritance graph is
      // correct. We proceed to check the contents of these classes.
      prog.classes.foreach{ c : ClassDecl =>  collectInClass(c,global) }

      def collectInClass(c: ClassDecl,gs : GlobalScope) = {
        // Traverse a class to collect symbols and emit errors
        // in case a correctness rule of Tool is violated
        // Note: It is important that you analyze parent classes first (Why?)
        c.methods.foreach { meth: MethodDecl => collectMethods(meth,gs,c.getSymbol) }
        c.vars.foreach { v: VarDecl =>
          val vs: VariableSymbol = new VariableSymbol(v.id.value)
          vs.setPos(v); v.setSymbol(vs); v.id.setSymbol(vs)

          c.getSymbol.lookupVar(v.id.value) match {
            case None => c.getSymbol.members += (v.id.value -> vs)
            case _ => error("Redefinition of class variable '" + v.id.value + "' at " + v.id.position,vs)
          }
        }
      }

      def collectMethods(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol) = {
        val ms: MethodSymbol = new MethodSymbol(meth.id.value, cs)
        ms.setPos(meth); meth.setSymbol(ms); meth.id.setSymbol(ms);

        cs.methods.get(meth.id.value) match {
          case None => cs.methods += (meth.id.value -> ms)
          case _ => error("Redefinition of method " + meth.id.value + " at " + meth.id.position,cs)
        }

        meth.args.foreach { a: Formal =>
          val as: VariableSymbol = new VariableSymbol(a.id.value)
          as.setPos(a); a.setSymbol(as); a.id.setSymbol(as)

          meth.getSymbol.params.get(a.id.value) match {
            case None =>
              meth.getSymbol.argList ++= List(as)
              meth.getSymbol.params += (a.id.value -> as)
            case _ => error("Argument name '" + a.id.value + "' used twice at " + a.id.position,meth)
          }
        }

        meth.vars.foreach { v: VarDecl =>
          val vs: VariableSymbol = new VariableSymbol(v.id.value)
          vs.setPos(v); v.setSymbol(vs); v.id.setSymbol(vs)

          meth.getSymbol.members.get(v.id.value) match {
            case None => meth.getSymbol.members += (v.id.value -> vs)
            case _ => error("Redefinition of local variable " + v.id.value + " at " + v.id.position,meth)
          }
        }
      }

      //Check field overriding
      global.classes.values.foreach { cldecl => traverseInheritanceChain(cldecl, cldecl.members)}
      def traverseInheritanceChain(baseClass : ClassSymbol, forbiddenNames: Map[String, VariableSymbol]): Unit = {
        baseClass.parent match {
          case None =>
          case Some(parentSymbol) =>
            (forbiddenNames.keys.toList intersect parentSymbol.members.keys.toList) foreach {
              overridingField => error("Field '" + overridingField + "' overrides parent's field ", forbiddenNames(overridingField))
            }
            traverseInheritanceChain(parentSymbol, parentSymbol.members ++: forbiddenNames)
        }
      }

      //check shadowing
      global.classes.values.foreach{ cs =>
        cs.methods.values.foreach{ms =>
          ((ms.params.keys toSet) intersect (ms.members.keys toSet)).foreach{shadowed =>
            error("Variable '" + shadowed + "' shadows a method parameter in method " + ms.name, ms.members(shadowed))
          }
        }
      }

      global
    }

    def setPSymbols(prog: Program, gs: GlobalScope) = {
      // Traverse within each definition of the program
      // and attach symbols to Identifiers and "this"
      prog.main.stats foreach { setSSymbols(_)(gs,None) }
      prog.classes foreach {
        _.methods foreach { meth => meth.stats foreach (setSSymbols(_)(gs,Some(meth.getSymbol)))}
      }
      prog.classes.foreach{ cldcl => setCSymbols(cldcl,gs) }
    }

    def setCSymbols(klass: ClassDecl, gs: GlobalScope) = {
      val classSym = gs.lookupClass(klass.id.value).get
      for (varDecl <- klass.vars) { setTypeSymbol(varDecl.tpe, gs)}

      klass.methods.foreach(setMSymbols(_, gs, classSym))
    }

    def setMSymbols(meth: MethodDecl, gs: GlobalScope, cs: ClassSymbol) = {
      meth.vars foreach { vrdcl => setTypeSymbol(vrdcl.tpe,gs)}
      setTypeSymbol(meth.retType,gs)
      setESymbols(meth.retExpr)(gs,Some(meth.getSymbol))
      meth.args foreach (arg => setTypeSymbol(arg.tpe,gs))
    }

    def setSSymbols(stat: StatTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]): Unit = stat match {
      case Block(stats: List[StatTree]) => stats.foreach(setSSymbols(_))
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        setESymbols(expr)
        setSSymbols(thn)
        if (!els.isEmpty) setSSymbols(els.get)
      case While(expr: ExprTree, stat: StatTree) =>
        setESymbols(expr)
        setSSymbols(stat)
      case Println(expr: ExprTree) => setESymbols(expr)
      case Assign(id: Identifier, expr: ExprTree) =>
        setISymbol(id)
        setESymbols(expr)
      case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
        setISymbol(id)
        setESymbols(index)
        setESymbols(expr)
      case DoExpr(expr: ExprTree) =>
        setESymbols(expr)
    }

    def setESymbols(expr: ExprTree)(implicit gs: GlobalScope, ms: Option[MethodSymbol]) : Unit = expr match {
      case And(lhs: ExprTree, rhs: ExprTree) => setESymbols(lhs); setESymbols(rhs)
      case Or(lhs: ExprTree, rhs: ExprTree) => setESymbols(lhs); setESymbols(rhs)
      case Not(expr: ExprTree) => setESymbols(expr)
      case Plus(lhs: ExprTree, rhs: ExprTree) => setESymbols(lhs); setESymbols(rhs)
      case Minus(lhs: ExprTree, rhs: ExprTree) => setESymbols(lhs); setESymbols(rhs)
      case Times(lhs: ExprTree, rhs: ExprTree) => setESymbols(lhs); setESymbols(rhs)
      case Div(lhs: ExprTree, rhs: ExprTree) => setESymbols(lhs); setESymbols(rhs)
      case LessThan(lhs: ExprTree, rhs: ExprTree) => setESymbols(lhs); setESymbols(rhs)
      case Equals(lhs: ExprTree, rhs: ExprTree) => setESymbols(lhs); setESymbols(rhs)
      case ArrayRead(arr: ExprTree, index: ExprTree) => setESymbols(arr); setESymbols(index)
      case ArrayLength(arr: ExprTree) => setESymbols(arr)
      case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => setESymbols(obj); args.foreach(setESymbols(_))
      case NewIntArray(size: ExprTree) => setESymbols(size)
      case New(tpe: Identifier) => gs.lookupClass(tpe.value) match {
        case None => error("Undefined class " + tpe.value + " at " + tpe.position,tpe)
        case _ => tpe.setSymbol(gs.lookupClass(tpe.value).get)
      }
      case thisym: This => ms match {
        case None => error("This used in main object",thisym)
        case Some(meth) => thisym.setSymbol(meth.classSymbol)
      }
      case Variable(id) => setISymbol(id)
      case _ =>
    }

    def setISymbol(id: Identifier)(implicit ms: Option[MethodSymbol]) = {
      // in this context, it will always be an expression (variable)
      ms.flatMap(_.lookupVar(id.value)) match {
        case None => error("Undeclared identifier: " + id.value + " at " + id.position,id)
        case Some(sym) =>
          id.setSymbol(sym)
      }
    }

    def setTypeSymbol(tpe: TypeTree, gs: GlobalScope): Unit = {
      tpe match {
        case ClassType(id) =>
          gs.lookupClass(id.value) match {
            case None => error("Undeclared class type " + id.value + " at " + id.position, id)
            case Some(cs) =>
              cs.setPos(id); id.setSymbol(cs)
          }
        case _ =>
      }
    }

    def setTypes(global : GlobalScope): Unit = {
      prog.classes.foreach { c: ClassDecl =>
        c.vars.foreach { v: VarDecl => v.getSymbol.setType(v.tpe.getType) }
        c.methods.foreach { m: MethodDecl =>
          m.getSymbol.setType(m.retType.getType)
          m.args.foreach { f: Formal => f.getSymbol.setType(f.tpe.getType) }
          m.vars.foreach { v: VarDecl => v.getSymbol.setType(v.tpe.getType) }
        }
      }
    }

    def checkOverloading(global : GlobalScope): Unit ={
      global.classes.values.foreach{ cs =>
        cs.methods.values.foreach{ms => traverseInheritanceChainForOverloading(cs,ms)}
      }
      def traverseInheritanceChainForOverloading(baseClass : ClassSymbol,ms : MethodSymbol) : Unit = {
        baseClass.parent match {
          case None =>
          case Some(parentSymbol) =>
            parentSymbol.lookupMethod(ms.name) match {
              case None =>
              case Some(parentms) =>
                if (parentms.argList.size != ms.argList.size)
                  error("Method " + ms.name + " overloaded at " + ms.position + " from superclass method " + parentms.position + ". Different number of arguments.", ms)
                else if(ms.getType != parentms.getType)
                  error("Method " + ms.name + " overloaded at " + ms.position + " from superclass method " + parentms.position + ". Different return type.", ms)
                else
                  (ms.params zip parentms.params) foreach { case (arg, parentarg) =>
                    if (arg._2.getType != parentarg._2.getType)
                      error("Method " + ms.name + " overloaded at " + ms.position + " from superclass method " + parentms.position + ". Different argument types.", ms)
                  }
            }
            traverseInheritanceChainForOverloading(parentSymbol,ms)
        }
      }
    }
    val gs = collectSymbols(prog)

    terminateIfErrors()

    setPSymbols(prog, gs)

    setTypes(gs)

    checkOverloading(gs)

    prog
  }
}


