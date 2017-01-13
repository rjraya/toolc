package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {
 
  // A mapping from a parameter/local variable name to the index of this parameter/variable
  // in the fields of a method
  type LocalsPosMapping = Map[String,Int]

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /**** Helper methods ****/

    def generateClassFile(ct: ClassDecl, shortFileName: String, outDir: String): Unit = {
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor
      // Add class fields
      ct.vars.foreach { field: VarDecl => cf.addField(typeToDescr(field.tpe.getType), field.id.value);}
      //Add class methods and generate code for them
      ct.methods.foreach { method: MethodDecl =>
        val ms = method.getSymbol
        val ch: CodeHandler = cf.addMethod(typeToDescr(ms.getType), ms.name, ms.argList.map { arg: VariableSymbol =>
          typeToDescr(arg.getType)
        }).codeHandler
        cGenMethod(ch, method)
      }

      writeClassFile(cf, outDir, cs.name)
    }

    def generateMainClassFile(main: MainObject, sourceFileName: String, outDir: String): Unit = {
      // Main class has a special handling
      val cs = main.getSymbol
      val mainClassFile = new ClassFile(cs.name, None)
      mainClassFile.setSourceFile(sourceFileName)
      mainClassFile.addDefaultConstructor

      cGenMain(
        mainClassFile.addMainMethod.codeHandler,
        prog.main.stats,
        cs.name
      )

      writeClassFile(mainClassFile, outDir, cs.name)
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def writeClassFile(cf: ClassFile, dir: String, className: String) = {
      try {
        cf.writeToFile(dir + className + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }


    def cGenMethod(ch: CodeHandler, mt: MethodDecl): Unit = {
      val ms = mt.getSymbol

      // Maps each argument to one local variable index position
      val argMappings = mt.args.zipWithIndex.map { case (arg, index) =>
        arg.id.getSymbol.name -> (index + 1)
      }.toMap

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map( v => v.getSymbol.name -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings

      mt.stats map {x => cGenStat(x)(ch,mapping,ms.classSymbol.name)} //generate code for statements

      cGenExpr(mt.retExpr)(ch,mapping,ms.classSymbol.name) // Generate code for the return expression
      ch << { //Return with the correct opcode, based on the type of the return expression
        mt.retExpr.getType match {
          case TInt | TBoolean => IRETURN
          case _ => ARETURN
        }
      }

      ch.freeze
    }

    // Generates code for the main method
    def cGenMain(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      stmts map {x => cGenStat(x)(ch,Map(),cname)}
      ch << RETURN
      ch.freeze
    }

    // Generates code for a statement
    def cGenStat(statement: StatTree)
                (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      statement match {
        case Block(stats) => stats foreach cGenStat
        case If(cond: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          val lafter: String = ch.getFreshLabel("after")
          val lelse: String = ch.getFreshLabel("else")
          cGenExpr(cond)
          if (els.isDefined) {
            ch << IfEq(lelse)
            cGenStat(thn)
            ch << Goto(lafter)
            ch << Label(lelse)
            cGenStat(els.get)
            ch << Label(lafter)
          } else {
            ch << IfEq(lafter)
            cGenStat(thn)
            ch << Label(lafter)
          }
        case While(expr: ExprTree, stat: StatTree) =>
          val lwhile = ch.getFreshLabel("while")
          val lafter = ch.getFreshLabel("after")
          ch << Label(lwhile)
          cGenExpr(expr)
          ch << IfEq(lafter)
          cGenStat(stat)
          ch << Goto(lwhile)
          ch << Label(lafter)
        case Println(expr: ExprTree) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          cGenExpr(expr)
          expr.getType match {
            case TInt => ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
            case TBoolean => ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V")
            case TString => ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
            case _ => sys.error("Wrong type in println during code generation. Should be int, boolean or string")
          }
        case Assign(id: Identifier, expr: ExprTree) =>
          if (mapping.contains(id.value)) { //local variable or argument
            cGenExpr(expr)
            ch << {
              id.getType match {
                case TInt | TBoolean => IStore(mapping(id.value))
                case TIntArray | TString | TClass(_) => AStore(mapping(id.value))
                case _ => sys.error("Wrong type in assign expression during code generation.")
              }
            }
          } else { //field
            ch << ALOAD_0 //this reference
            cGenExpr(expr)
            ch << PutField(cname,id.value,typeToDescr(id.getType))
          }
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          if(mapping.contains(id.value)) {
            ch << ALoad(mapping(id.value))
          } else {
            ch << ALOAD_0
            ch << GetField(cname, id.value, "[I")
          }
          cGenExpr(index)
          cGenExpr(expr)
          ch << IASTORE
        case DoExpr(expr) =>
          cGenExpr(expr)
          ch << POP
      }
    }

    // Generates code for an expression
    def cGenExpr(expr: ExprTree)
                (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = {
      expr match {
        case And(lhs,rhs) =>
          val lafter = ch.getFreshLabel("alreadyFalse")
          ch << ICONST_0
          cGenExpr(lhs)
          ch << IfEq(lafter)
          ch << POP
          cGenExpr(rhs)
          ch << Label(lafter)
        case Or(lhs,rhs) =>
          val lafter = ch.getFreshLabel("after")
          ch << ICONST_1
          cGenExpr(lhs)
          ch << IfNe(lafter)
          ch << POP
          cGenExpr(rhs)
          ch << Label(lafter)
        case Plus(lhs,rhs) => (lhs.getType,rhs.getType) match {
          case (TInt,TInt) => cGenExpr(lhs); cGenExpr(rhs); ch << IADD
          case (TInt,TString) =>
            ch << DefaultNew("java/lang/StringBuilder")
            cGenExpr(lhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
            cGenExpr(rhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          case (TString,TInt) =>
            ch << DefaultNew("java/lang/StringBuilder")
            cGenExpr(lhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append","(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            cGenExpr(rhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          case (TString,TString) =>
            ch << DefaultNew("java/lang/StringBuilder")
            cGenExpr(lhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            cGenExpr(rhs)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          case _ => sys.error("Wrong type for plus expression during code generation")
        }
        case Minus(lhs,rhs) => cGenExpr(lhs); cGenExpr(rhs); ch << ISUB
        case Times(lhs,rhs) => cGenExpr(lhs); cGenExpr(rhs); ch << IMUL
        case Div(lhs: ExprTree, rhs: ExprTree) => cGenExpr(lhs); cGenExpr(rhs); ch << IDIV
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
          val lafter = ch.getFreshLabel("after")
          ch << ICONST_1
          cGenExpr(lhs)
          cGenExpr(rhs)
          ch << If_ICmpLt(lafter)
          ch << POP
          ch << ICONST_0
          ch << Label(lafter)
        case Equals(lhs,rhs) =>
          val lafter = ch.getFreshLabel("after")

          ch << ICONST_1
          cGenExpr(lhs)
          cGenExpr(rhs)
          lhs.getType match {
            case TInt | TBoolean => ch << If_ICmpEq(lafter)
            case _ => ch << If_ACmpEq(lafter)
          }
          ch << POP
          ch << ICONST_0
          ch << Label(lafter)
        case ArrayRead(arr,index) => cGenExpr(arr); cGenExpr(index); ch << IALOAD
        case ArrayLength(arr) => cGenExpr(arr); ch << ARRAYLENGTH
        case MethodCall(obj, meth: Identifier, args: List[ExprTree]) =>
          val ms = meth.getSymbol match {
            case m: MethodSymbol => m
            case _ => sys.error("Wrong method identifier during code generation")
          }
          val signature = "(" + ms.argList.foldLeft("")((sig,arg) => sig + typeToDescr(arg.getType)) + ")" + typeToDescr(ms.getType)
          cGenExpr(obj)
          args foreach cGenExpr
          ch << InvokeVirtual(ms.classSymbol.name, meth.value,signature)
        case IntLit(value: Int) => ch << Ldc(value)
        case StringLit(value: String) => ch << Ldc(value)
        case True() => ch << ICONST_1
        case False() => ch << ICONST_0
        case Variable(id) =>
          if (mapping.contains(id.value)) {
            id.getType match {
              case TInt | TBoolean => ch << ILoad(mapping(id.value))
              case _ => ch << ALoad(mapping(id.value))
            }
          } else {
            ch << ALOAD_0
            ch << GetField(cname, id.value, typeToDescr(id.getType))
          }
        case This() => ch << ALOAD_0
        case NewIntArray(size: ExprTree) => cGenExpr(size); ch << NewArray(NewArray.types("T_INT"))
        case New(tpe: Identifier) => ch << DefaultNew(tpe.value)
        case Not(expr: ExprTree) =>
          val lafter = ch.getFreshLabel("after")
          ch << ICONST_1
          cGenExpr(expr)
          ch << IfEq(lafter)
          ch << POP
          ch << ICONST_0
          ch << Label(lafter)
      }

    }


    // Transforms a Tool type to the corresponding JVM type description
    def typeToDescr(t: Type): String = t match {
      case TInt => "I"
      case TBoolean => "Z"
      case TIntArray => return "[I"
      case TString => "Ljava/lang/String;"
      case obj : TClass => return "L" + obj.classSymbol.name + ";"
      case _ => sys.error("Wrong type passed to code generation")
    }

    /**** Main code ****/

    // Make output directory
    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    // Name of source file (to track positions)
    val sourceName = ctx.files.head.getName

    // output class code
    prog.classes foreach {
      generateClassFile(_, sourceName, outDir)
    }

    // output main object code
    generateMainClassFile(prog.main, sourceName, outDir)

  }
}