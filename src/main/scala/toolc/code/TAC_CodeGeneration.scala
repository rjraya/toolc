package toolc
package code

//Warning: this code is not used in the current implementation of the analysis framework.
//         it is kept here only for debugging purposes in the future.

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._
import toolc.abstract_inter.tac._


object TAC_CodeGeneration extends Pipeline[TAC_Program, Unit] {

  // A mapping from a parameter/local variable name to the index of this parameter/variable
  // in the fields of a method
  type LocalsPosMapping = Map[String,Int]

  def run(ctx: Context)(prog: TAC_Program): Unit = {
    import ctx.reporter._

    /** ** Helper methods ****/

    def generateClassFile(tcd: TAC_ClassDecl, shortFileName: String, outDir: String): Unit = {
      val ct = tcd.cdecl
      val cs = ct.getSymbol
      val cf = new ClassFile(cs.name, cs.parent.map(_.name))
      cf.setSourceFile(shortFileName)
      cf.addDefaultConstructor
      // Add class fields
      ct.vars.foreach { field: VarDecl => cf.addField(typeToDescr(field.tpe.getType), field.id.value); }
      //Add class methods and generate code for them
      tcd.methods.foreach { met: TAC_MethodDecl =>
        val method = met.meth
        val ms = method.getSymbol
        val ch: CodeHandler = cf.addMethod(typeToDescr(ms.getType), ms.name, ms.argList.map { arg: VariableSymbol =>
          typeToDescr(arg.getType)
        }).codeHandler
        cGenMethod(ch, met)
      }

      writeClassFile(cf, outDir, cs.name)
    }

    def generateMainClassFile(tmain: TAC_MainObject, sourceFileName: String, outDir: String): Unit = {
      val main = tmain.main
      val cs = main.getSymbol
      val mainClassFile = new ClassFile(cs.name, None)
      mainClassFile.setSourceFile(sourceFileName)
      mainClassFile.addDefaultConstructor

      cGenMain(mainClassFile.addMainMethod.codeHandler, prog.main.stats,cs.name,tmain.numTempVariables)

      writeClassFile(mainClassFile, outDir, cs.name)
    }

    // Generates code for the main method
    def cGenMain(ch: CodeHandler, stmts: List[TAC_Instr], cname: String,numTempVariables:Int): Unit = {
      val tempVariableMappings = (0 to numTempVariables).map(i => "t"+(i+1) -> ch.getFreshVar).toMap
      stmts map { x => cGenTACInstr(x)(ch, tempVariableMappings, cname) }
      ch << RETURN
      ch.freeze
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def writeClassFile(cf: ClassFile, dir: String, className: String) = {
      try {
        cf.writeToFile(dir + className + ".class")
      } catch {
        case e: Exception => fatal(e.getMessage)
      }
    }

    def cGenMethod(ch: CodeHandler, tmd: TAC_MethodDecl): Unit = {
      val mt = tmd.meth
      val ms = mt.getSymbol

      // Maps each argument to one local variable index position
      val argMappings = mt.args.zipWithIndex.map { case (arg, index) => arg.id.getSymbol.name -> (index + 1)}.toMap

      // Maps each variable to one local variable index position
      val variableMappings = mt.vars.map(v => v.getSymbol.name -> ch.getFreshVar).toMap

      //Assuming no variable is named as t number
      val tempVariableMappings = (0 to tmd.numTempVariables).map(i => "t"+(i+1) -> ch.getFreshVar).toMap

      val mapping = argMappings ++ variableMappings ++ tempVariableMappings

      tmd.tacList map {x => cGenTACInstr(x)(ch, mapping, ms.classSymbol.name)}//generate code for statements

      ch.freeze
    }

    def assignToTemp(temp : TAC_Temp)
                    (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String)= {
      ch << {
        temp.tpe match {
          case TInt | TBoolean => IStore(mapping(temp.name))
          case TIntArray | TString | TClass(_) => AStore(mapping(temp.name))
          case _ => sys.error("Wrong type in assign expression during code generation.")
        }
      }
    }
    def cGenTACInstr(tacInstr: TAC_Instr)
                    (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = tacInstr match {
      case TAC_Label(l, _) => ch << new Label(l)
      case TAC_Jump(l, _) => ch << Goto(l.label)
      case TAC_CJump(l, cond:TAC_Op, _) => cGenTACOp(cond); ch << IfNe(l.label)
      case TAC_Ret(value:TAC_Op,_) =>
        cGenTACOp(value)
        ch << {
          value.getType match {
            case TInt | TBoolean => IRETURN
            case _ => ARETURN
          }
        }
      case TAC_ArrayLength(lhs:TAC_Op, array: TAC_Op,_) => cGenTACOp(array); ch << ARRAYLENGTH; assignToTemp(lhs)
      case TAC_NewClass(lhs:TAC_Op,tpe, _) => ch << DefaultNew(tpe.value); assignToTemp(lhs)
      case TAC_NewIntArray(lhs,size: TAC_Op, _) => cGenTACOp(size); ch << NewArray(NewArray.types("T_INT")); assignToTemp(lhs)
      case mc@TAC_MethodCall(lhs, receiver: TAC_Op, meth, args: List[TAC_Op], _) =>
        val ms = meth.getSymbol match {
          case m: MethodSymbol => m
          case _ => sys.error("Wrong method identifier during code generation")
        }
        val signature = "(" + ms.argList.foldLeft("")((sig, arg) => sig + typeToDescr(arg.getType)) + ")" + typeToDescr(ms.getType)
        cGenTACOp(receiver)
        args foreach cGenTACOp
        ch << InvokeVirtual(ms.classSymbol.name, meth.value, signature)
        assignToTemp(lhs)
      case TAC_PrintLn(param:TAC_Op, _) =>
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        cGenTACOp(param)
        param.getType match {
          case TInt => ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
          case TBoolean => ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V")
          case TString => ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
          case _ => sys.error("Wrong type in println during code generation. Should be int, boolean or string");
        }
      case TAC_DoExpr(param:TAC_Op, _) => cGenTACOp(param); ch << POP
      case TAC_Assign(lhs:TAC_Op, rhs:TAC_Op, _) => lhs match {
        case TAC_Var(id: Identifier) =>
          if (mapping.contains(id.value)) {
            cGenTACOp(rhs)
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
            cGenTACOp(rhs)
            ch << PutField(cname, id.value, typeToDescr(id.getType))
          }
        //temp variable cannot be a field
        case at@TAC_Temp(name, tpe) =>
          cGenTACOp(rhs)
          assignToTemp(at)
        case _ => sys.error("Assignment to a literal in code generation.")
      }
      case TAC_ArrayAssign(array:TAC_Op, arrindex:TAC_Op, value:TAC_Op, _) =>
        if (mapping.contains(array.id.value)) {
          ch << ALoad(mapping(array.id.value))
        } else {
          ch << ALOAD_0
          ch << GetField(cname, array.id.value, "[I")
        }
        cGenTACOp(arrindex)
        cGenTACOp(value)
        ch << IASTORE
      case TAC_ArrayRead(lhs, array: TAC_Op,arrindex: TAC_Op,_) =>
        cGenTACOp(array); cGenTACOp(arrindex); ch << IALOAD; assignToTemp(lhs)
      case TAC_BinOp(lhs,lexpr: TAC_Op,rexpr: TAC_Op,binop,_) =>
        binop match{
          case "&&" =>
            val lafter = ch.getFreshLabel("alreadyFalse")
            ch << ICONST_0
            cGenTACOp(lexpr)
            ch << IfEq(lafter)
            ch << POP
            cGenTACOp(rexpr)
            ch << Label(lafter)
          case "||" =>
            val lafter = ch.getFreshLabel("after")
            ch << ICONST_1
            cGenTACOp(lexpr)
            ch << IfNe(lafter)
            ch << POP
            cGenTACOp(rexpr)
            ch << Label(lafter)
          case "+" => (lexpr.getType, rexpr.getType) match {
            case (TInt, TInt) => cGenTACOp(lexpr); cGenTACOp(rexpr); ch << IADD
              case (TInt, TString) =>
                ch << DefaultNew("java/lang/StringBuilder")
                cGenTACOp(lexpr)
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
                cGenTACOp(rexpr)
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
                ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
              case (TString, TInt) =>
                ch << DefaultNew("java/lang/StringBuilder")
                cGenTACOp(lexpr)
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
                cGenTACOp(rexpr)
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
                ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
              case (TString, TString) =>
                ch << DefaultNew("java/lang/StringBuilder")
                cGenTACOp(lexpr)
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
                cGenTACOp(rexpr)
                ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
                ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
              case _ => sys.error("Wrong type for plus expression during code generation")
            }
          case "-" => cGenTACOp(lexpr); cGenTACOp(rexpr); ch << ISUB
          case "*" => cGenTACOp(lexpr); cGenTACOp(rexpr); ch << IMUL
          case "/" => cGenTACOp(lexpr); cGenTACOp(rexpr); ch << IDIV
          case "<" =>
            val lafter = ch.getFreshLabel("after")
            ch << ICONST_1
            cGenTACOp(lexpr)
            cGenTACOp(rexpr)
            ch << If_ICmpLt(lafter)
            ch << POP
            ch << ICONST_0
            ch << Label(lafter)
          case "==" =>
            val lafter = ch.getFreshLabel("after")

            ch << ICONST_1
            cGenTACOp(lexpr)
            cGenTACOp(rexpr)
            lexpr.getType match {
              case TInt | TBoolean => ch << If_ICmpEq(lafter)
              case _ => ch << If_ACmpEq(lafter)
            }
            ch << POP
            ch << ICONST_0
            ch << Label(lafter)
        }
        assignToTemp(lhs)
      case TAC_UnOp(lhs:TAC_Op, rhs:TAC_Op, unop: String,_) => unop match {
        case "!" =>
          val lafter = ch.getFreshLabel("after")
          ch << ICONST_1
          cGenTACOp(rhs)
          ch << IfEq(lafter)
          ch << POP
          ch << ICONST_0
          ch << Label(lafter)
          assignToTemp(lhs)
        case _ =>
      }
      case TAC_Comment(_,_) =>
    }


    def cGenTACOp(tacOp:TAC_Op)
                 (implicit ch: CodeHandler, mapping: LocalsPosMapping, cname: String): Unit = tacOp match {
      case TAC_Var(id:Identifier) =>
        if (mapping.contains(id.value)) {
          id.getType match {
            case TInt | TBoolean => ch << ILoad(mapping(id.value))
            case _ => ch << ALoad(mapping(id.value))
          }
        } else {
          ch << ALOAD_0
          ch << GetField(cname, id.value, typeToDescr(id.getType))
        }
      case TAC_Temp(name,tpe) =>
        tpe match {
          case TInt | TBoolean => ch << ILoad(mapping(name))
          case _ => ch << ALoad(mapping(name))
        }
      case TAC_Lit(lit_node: Literal,_) => lit_node match{
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