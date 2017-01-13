package toolc
package code

import toolc.abstract_inter.untac._
import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object UNTAC_CodeGeneration extends Pipeline[UNTAC_Program, Unit] {
  type LocalsPosMapping = Map[String,Int]

  def run(ctx: Context)(prog: UNTAC_Program): Unit = {
    import ctx.reporter._

    /** ** Helper methods ****/
    def generateClassFile(tcd: UNTAC_ClassDecl, shortFileName: String, outDir: String): Unit = {
      val ct = tcd.cdecl
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor
      // Add class fields
      ct.vars.foreach { field: VarDecl => cf.addField(typeToDescr(field.tpe.getType), field.id.value); }
      //Add class methods and generate code for them
      tcd.methods.foreach { met: UNTAC_MethodDecl =>
        val method = met.meth
        val ms = method.getSymbol
        val ch: CodeHandler = cf.addMethod(typeToDescr(ms.getType), ms.name, ms.argList.map { arg: VariableSymbol =>
          typeToDescr(arg.getType)
        }).codeHandler
        cGenMethod(ch, met,met.temps)
      }

      writeClassFile(cf, outDir, cs.name)
    }

    def generateMainClassFile(tmain: UNTAC_MainObject, sourceFileName: String, outDir: String): Unit = {
      val main = tmain.main
      val cs = main.getSymbol
      val mainClassFile = new ClassFile(cs.name, None)
      mainClassFile.setSourceFile(sourceFileName)
      mainClassFile.addDefaultConstructor

      cGenMain(mainClassFile.addMainMethod.codeHandler, prog.main.stats,cs.name,prog.main.temps)

      writeClassFile(mainClassFile, outDir, cs.name)
    }

    def cGenMain(ch: CodeHandler, stmts: List[UNTAC_Instr], cname: String,temps : List[UNTAC_Temp]): Unit = {
      val tempVariableMappings = temps.map(t => t.name -> ch.getFreshVar).toMap
      stmts map { x => cGenTAC_Instr(x)(ch,tempVariableMappings, cname) }
      ch << RETURN
      ch.freeze
    }

    def writeClassFile(cf: ClassFile, dir: String, className: String) = {
      try {
        cf.writeToFile(dir + className + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }

    def cGenMethod(ch: CodeHandler, tmd: UNTAC_MethodDecl,temps:List[UNTAC_Temp]): Unit = {
      val mt = tmd.meth
      val ms = mt.getSymbol
      val argMappings = mt.args.zipWithIndex.map{case (arg, index) => arg.id.getSymbol.name -> (index + 1)}.toMap
      val variableMappings = mt.vars.map(v => v.getSymbol.name -> ch.getFreshVar).toMap
      val tempVariableMappings = temps.map(t => t.name -> ch.getFreshVar).toMap
      val mapping = argMappings ++ variableMappings ++ tempVariableMappings
      tmd.tacList map {x => cGenTAC_Instr(x)(ch, mapping, ms.classSymbol.name)}
      ch.freeze
    }

    def cGenTAC_Instr(tacInstr: UNTAC_Instr)(implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = tacInstr match {
      case UNTAC_Label(l) => ch << new Label(l)
      case UNTAC_Jump(l) => ch << Goto(l.label)
      case UNTAC_CJump(l,cond) => cGenTAC_Instr(cond); ch << IfNe(l.label)
      case UNTAC_Ret(value) =>
        cGenTAC_Instr(value)
        ch << {
          value.getType match {
            case TInt | TBoolean => IRETURN
            case _ => ARETURN
          }
        }
      case UNTAC_ArrayLength(_,array) => cGenTAC_Instr(array); ch << ARRAYLENGTH
      case UNTAC_NewClass(_,tpe) => ch << DefaultNew(tpe.value)
      case UNTAC_NewIntArray(_,size) => cGenTAC_Instr(size); ch << NewArray(NewArray.types("T_INT"))
      case mc@UNTAC_MethodCall(lhs,receiver, meth,args) =>
        val ms = meth.getSymbol match {
          case m: MethodSymbol => m
          case _ => sys.error("Wrong method identifier during code generation")
        }
        val signature = "(" + ms.argList.foldLeft("")((sig, arg) => sig + typeToDescr(arg.getType)) + ")" + typeToDescr(ms.getType)
        cGenTAC_Instr(receiver)
        args foreach cGenTAC_Instr
        ch << InvokeVirtual(ms.classSymbol.name, meth.value, signature)
        if(lhs == None) ch << POP
      case UNTAC_PrintLn(param) =>
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        cGenTAC_Instr(param)
        param.getType match {
          case TInt => ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
          case TBoolean => ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V")
          case TString => ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
          case _ => sys.error("Wrong type in println during code generation. Should be int, boolean or string");
        }
      case UNTAC_DoExpr(param) => cGenTAC_Instr(param); ch << POP
      case UNTAC_Assign(_,lhs,rhs) => lhs match {
        case Some(UNTAC_Var(id: Identifier)) =>
          if (mapping.contains(id.value)) {
            cGenTAC_Instr(rhs)
            ch << {
              id.getType match {
                case TInt | TBoolean => IStore(mapping(id.value))
                case TIntArray | TString | TClass(_) => AStore(mapping(id.value))
                case _ => sys.error("Wrong type in assign expression during code generation.")
              }
            }
          } else {
            //field
            ch << ALOAD_0 //this reference
            cGenTAC_Instr(rhs)
            ch << PutField(cname, id.value, typeToDescr(id.getType))
          }
        case _ => sys.error("Assignment to literal or temporal during code generation other than method call")
      }
      case UNTAC_ArrayAssign(array,arrindex,value) =>
        if (mapping.contains(array.id.value)) {
          ch << ALoad(mapping(array.id.value))
        } else {
          ch << ALOAD_0
          ch << GetField(cname, array.id.value, "[I")
        }
        cGenTAC_Instr(arrindex)
        cGenTAC_Instr(value)
        ch << IASTORE
      case UNTAC_ArrayRead(lhs,array,arrindex) =>
        cGenTAC_Instr(array); cGenTAC_Instr(arrindex); ch << IALOAD
      case UNTAC_BinOp(_,lexpr,rexpr,binop) =>
        binop match{
          case "&&" =>
            val lafter = ch.getFreshLabel("alreadyFalse")
            ch << ICONST_0
            cGenTAC_Instr(lexpr)
            ch << IfEq(lafter)
            ch << POP
            cGenTAC_Instr(rexpr)
            ch << Label(lafter)
          case "||" =>
            val lafter = ch.getFreshLabel("after")
            ch << ICONST_1
            cGenTAC_Instr(lexpr)
            ch << IfNe(lafter)
            ch << POP
            cGenTAC_Instr(rexpr)
            ch << Label(lafter)
          case "+" => (lexpr.getType, rexpr.getType) match {
            case (TInt, TInt) => cGenTAC_Instr(lexpr); cGenTAC_Instr(rexpr); ch << IADD
            case (TInt, TString) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenTAC_Instr(lexpr)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              cGenTAC_Instr(rexpr)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            case (TString, TInt) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenTAC_Instr(lexpr)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              cGenTAC_Instr(rexpr)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            case (TString, TString) =>
              ch << DefaultNew("java/lang/StringBuilder")
              cGenTAC_Instr(lexpr)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              cGenTAC_Instr(rexpr)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            case _ => sys.error("Wrong type for plus expression during code generation")
          }
          case "-" => cGenTAC_Instr(lexpr); cGenTAC_Instr(rexpr); ch << ISUB
          case "*" => cGenTAC_Instr(lexpr); cGenTAC_Instr(rexpr); ch << IMUL
          case "/" => cGenTAC_Instr(lexpr); cGenTAC_Instr(rexpr); ch << IDIV
          case "<" =>
            val lafter = ch.getFreshLabel("after")
            ch << ICONST_1
            cGenTAC_Instr(lexpr)
            cGenTAC_Instr(rexpr)
            ch << If_ICmpLt(lafter)
            ch << POP
            ch << ICONST_0
            ch << Label(lafter)
          case "==" =>
            val lafter = ch.getFreshLabel("after")

            ch << ICONST_1
            cGenTAC_Instr(lexpr)
            cGenTAC_Instr(rexpr)
            lexpr.getType match {
              case TInt | TBoolean => ch << If_ICmpEq(lafter)
              case _ => ch << If_ACmpEq(lafter)
            }
            ch << POP
            ch << ICONST_0
            ch << Label(lafter)
      }
      case UNTAC_UnOp(_,rhs,unop) => unop match {
        case "!" =>
          val lafter = ch.getFreshLabel("after")
          ch << ICONST_1
          cGenTAC_Instr(rhs)
          ch << IfEq(lafter)
          ch << POP
          ch << ICONST_0
          ch << Label(lafter)
        case _ =>
      }
      case UNTAC_Comment(_) =>

      case v@UNTAC_Var(id) => cGenTACOp(v)
      case ln@UNTAC_Lit(lit_node) => cGenTACOp(ln)
      case UNTAC_Temp(name,tpe) => sys.error("Non temporal variables during code generation possible")
    }


    def cGenTACOp(tacOp:UNTAC_Op)(implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = tacOp match {
      case UNTAC_Var(id:Identifier) =>
        if (mapping.contains(id.value)) {
          id.getType match {
            case TInt | TBoolean => ch << ILoad(mapping(id.value))
            case _ => ch << ALoad(mapping(id.value))
          }
        } else {
          ch << ALOAD_0
          ch << GetField(cname, id.value, typeToDescr(id.getType))
        }
      case UNTAC_Temp(name,tpe) =>
        tpe match {
          case TInt | TBoolean => ch << ILoad(mapping(name))
          case _ => ch << ALoad(mapping(name))
        }
      case UNTAC_Lit(lit_node: Literal) => lit_node match{
        case IntLit(value: Int) => ch << Ldc(value)
        case StringLit(value: String) => ch << Ldc(value)
        case True() => ch << ICONST_1
        case False() => ch << ICONST_0
        case This() => ch << ALOAD_0
      }
    }

    // Transforms a Tool type to the corresponding JVM type description
    def typeToDescr(t: Type): String = t match {
      case TInt => "I"
      case TBoolean => "Z"
      case TIntArray => "[I"
      case TString => "Ljava/lang/String;"
      case obj: TClass => "L" + obj.classSymbol.name + ";"
      case _ => sys.error("Wrong type passed to code generation")
    }

    /** ** Main code ****/

    // Make output directory
    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")
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
