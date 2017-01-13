package toolc
package analyzer

import ast.Trees._
import Types.{TIntArray, _}
import toolc.analyzer.Symbols.ClassSymbol
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcClass(klass: ClassDecl): Unit = klass.methods.foreach(tcMethod)

    /** Type checks statements and return expression of the method */
    def tcMethod(meth: MethodDecl): Unit = {
      meth.stats.foreach(tcStat)
      tcExpr(meth.retExpr,meth.retExpr.getType)
      if(!meth.retExpr.getType.isSubTypeOf(meth.getSymbol.getType)){
        error( meth.id.value + " should return a " + meth.retType.getType, meth.retExpr)
      }
    }

    /** Checks that the expression is a subtype of the ones in expectedTps.
      * If it's not, prints an error message and returns the error type.
      * Also adds missing symbols to methods in MethodCalls
      */
    def tcExpr(expr: ExprTree, expectedTps: Type*): Type = {
      val tpe = expr match {
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean); tcExpr(rhs, TBoolean); TBoolean
        case Or(lhs: ExprTree, rhs: ExprTree) =>
          tcExpr(lhs, TBoolean); tcExpr(rhs, TBoolean); TBoolean
        case Plus(lhs: ExprTree, rhs: ExprTree) =>
          val tpe1 = tcExpr(lhs, TInt, TString)
          val tpe2 = tcExpr(rhs, TInt, TString)
          if (tpe1 == TString || tpe2 == TString) TString else TInt
        case Minus(lhs: ExprTree, rhs: ExprTree) =>
          tcExpr(lhs, TInt); tcExpr(rhs, TInt); TInt
        case Times(lhs: ExprTree, rhs: ExprTree) =>
          tcExpr(lhs, TInt); tcExpr(rhs, TInt); TInt
        case Div(lhs: ExprTree, rhs: ExprTree) =>
          tcExpr(lhs, TInt); tcExpr(rhs, TInt); TInt
        case LessThan(lhs: ExprTree, rhs: ExprTree) =>
          tcExpr(lhs, TInt); tcExpr(rhs, TInt); TBoolean
        case Equals(lhs: ExprTree, rhs: ExprTree) =>
          tcExpr(lhs, TObject, TInt, TString, TBoolean,TIntArray) match {
            case TClass(cs) => tcExpr(rhs, TObject)
            case TInt => tcExpr(rhs, TInt)
            case TString => tcExpr(rhs, TString)
            case TBoolean => tcExpr(rhs, TBoolean)
            case TIntArray => tcExpr(rhs, TIntArray)
            case _ => TError
          }
          TBoolean
        case ArrayRead(arr: ExprTree, index: ExprTree) =>
          tcExpr(arr, TIntArray); tcExpr(index, TInt); TInt
        case ArrayLength(arr: ExprTree) =>
          tcExpr(arr, TIntArray); TInt
        case IntLit(value: Int) => TInt
        case StringLit(value: String) => TString
        case True() => TBoolean
        case False() => TBoolean
        case thiz: This => thiz.getSymbol.getType
        case NewIntArray(size: ExprTree) =>
          tcExpr(size, TInt); TIntArray
        case New(tpe: Identifier) => tpe.getType
        case Not(expr: ExprTree) =>
          tcExpr(expr, TBoolean); TBoolean
        case Variable(id: Identifier) => id.getType
        case MethodCall(obj, meth, args) => {
          val objtpe = tcExpr(obj, TObject)
          objtpe match {
            case TClass(cs) =>
              cs.lookupMethod(meth.value) match {
                case Some(method) => {
                  if (args.length != method.params.size) {
                    error("Calling " + method.name + " of class " + cs.name + " with " + args.length + " parameters. It requires " + method.params.size)
                    TError
                  }else {
                    meth.setSymbol(method)
                    (args zip method.argList) foreach { pair => tcExpr(pair._1, pair._2.getType) }
                    method.getType
                  }
                }
                case None => {
                  error("Method " + meth.value + " not found in type " + cs.name)
                  TError
                }
              }
            case _ => objtpe
          }
        }
      }

      if (!expectedTps.toList.exists(expr.getType.isSubTypeOf)) {
        error("Type error: Expected: " + expectedTps.mkString(" or ") + s", found: ${expr.getType}", expr)
        expectedTps.head
      }else{
        tpe
      }
    }

    /** Invokes tcExpr as needed in the expressions of stat */
    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats: List[StatTree]) =>
          stats foreach{tcStat(_)}
        case If(cond: ExprTree, thn: StatTree, els: Option[StatTree]) =>
          tcExpr(cond, TBoolean); tcStat(thn)
          els match{
            case Some(elstat) => tcStat(elstat)
            case None =>
          }
        case While(cond: ExprTree, stat: StatTree) =>
          tcExpr(cond, TBoolean); tcStat(stat)
        case Println(expr: ExprTree) =>
          tcExpr(expr, TInt, TBoolean, TString)
        case Assign(id: Identifier, expr: ExprTree) =>
          tcExpr(expr, id.getType)
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
          tcExpr(Variable(id),TIntArray) ; tcExpr(index, TInt) ; tcExpr(expr, TInt)
        case DoExpr(expr: ExprTree) => tcExpr(expr,TInt, TBoolean, TString, TIntArray, TObject)
      }
    }
 
    prog.main.stats.foreach(tcStat)
    prog.classes.foreach(tcClass)

    prog
  }
}
