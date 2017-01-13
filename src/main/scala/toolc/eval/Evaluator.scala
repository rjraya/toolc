package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    val ectx = new MainContext
    prog.main.stats.foreach(evalStatement(_)(ectx))
  }

  def evalStatement(stmt: StatTree)(implicit ectx: EvaluationContext): Unit = stmt match {
    case Block(stats) => stats.foreach(evalStatement(_)(ectx))
    case If(expr, thn, els) => 
      val res = evalExpr(expr).asBool
      if(res) evalStatement(thn)
      else {
        els match {
          case Some(inst) => evalStatement(inst)
          case None =>
        }
      }
    case While(expr, stat) => while (evalExpr(expr).asBool) evalStatement(stat)
    case Println(expr) => 
      evalExpr(expr) match {
        case IntValue(i) => println(i)
        case StringValue(s) => println(s)
        case BoolValue(b) => println(b)
        case _ => fatal("Parameter for println() should be String, Int or Bool")
      }
    case Assign(id, expr) => ectx.setVariable(id.value, evalExpr(expr))
    case ArrayAssign(id, index, expr) =>
      val array = ectx.getVariable(id.value).asArray
      array.setIndex(evalExpr(index).asInt, evalExpr(expr).asInt)
    case DoExpr(expr) => 
      evalExpr(expr)(ectx)
      ()
  }

  def evalExpr(e: ExprTree)(implicit ectx: EvaluationContext): Value = e match {
    case IntLit(value) => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True() => BoolValue(true)
    case False() => BoolValue(false)
    case And(lhs, rhs) => BoolValue(evalExpr(lhs).asBool && evalExpr(rhs).asBool) 
    case Or(lhs, rhs)  => BoolValue(evalExpr(lhs).asBool || evalExpr(rhs).asBool)
    
    case Plus(lhs, rhs) => 
      var result : Value = null
      (evalExpr(lhs), evalExpr(rhs)) match {
        case (StringValue(l), StringValue(r)) => result = StringValue(l + r)
        case (IntValue(l), IntValue(r)) => result = IntValue(l + r)
        case (IntValue(l), StringValue(r)) => result = StringValue(l + r)
        case (StringValue(l), IntValue(r)) => result = StringValue(l + r)
        case _ => fatal("Operator + should take String or Int as parameters")
      }
      result
      
    case Minus(lhs, rhs) => IntValue(evalExpr(lhs).asInt - evalExpr(rhs).asInt)
    case Times(lhs, rhs) => IntValue(evalExpr(lhs).asInt * evalExpr(rhs).asInt)
    case Div(lhs, rhs) => IntValue(evalExpr(lhs).asInt / evalExpr(rhs).asInt)
    case LessThan(lhs, rhs) => BoolValue(evalExpr(lhs).asInt < evalExpr(rhs).asInt)
    case Not(expr) => BoolValue(!evalExpr(expr).asBool)
    
    case Equals(lhs, rhs) => 
      var result : Value = null
      (evalExpr(lhs), evalExpr(rhs)) match {
        case (IntValue(l), IntValue(r)) => result = BoolValue(l == r)
        case (BoolValue(l), BoolValue(r)) => result = BoolValue(l == r)
        case (lref, rref) => result = BoolValue(lref eq rref)
      }
      result
 
    case ArrayRead(arr, index) => IntValue(evalExpr(arr).asArray.getIndex(evalExpr(index).asInt))
    case ArrayLength(arr) => IntValue(evalExpr(arr).asArray.length)
    
    case MethodCall(obj, meth, args) => 
      val objValue = evalExpr(obj).asObject
      val mctx = new MethodContext(objValue)
      val md = findMethod(objValue.cd, meth.value)
      md.args.foreach(arg => mctx.declareVariable(arg.id.value))
      val pairs = md.args.zip(args)
      pairs.foreach(pair => mctx.setVariable(pair._1.id.value, evalExpr(pair._2)(ectx)))
      md.vars.foreach(v => mctx.declareVariable(v.id.value))
      md.stats.foreach(evalStatement(_)(mctx)) 
      evalExpr(md.retExpr)(mctx) 
      
    case Variable(Identifier(name)) => ectx.getVariable(name)
    
    case New(tpe) =>
      val cd = findClass(tpe.value);
      var obj = new ObjectValue(cd)
      fieldsOfClass(cd).foreach(obj.declareField(_))
      obj
      
    case This() => 
      ectx match {
        case mctx: MethodContext => mctx.obj
        case _ => fatal("Using this in main")
      }
      
    case NewIntArray(size) => new ArrayValue(new Array[Int](evalExpr(size).asInt))
  }

  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
  }

  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  class MainContext extends EvaluationContext {
    private def unavailable = fatal("The main object contains no variables and/or fields")
    def getVariable(name: String): Value          = unavailable
    def setVariable(name: String, v: Value): Unit = unavailable
    def declareVariable(name: String): Unit       = unavailable
  }

  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  sealed abstract class Value {
    private def expected(tp: String) = fatal(s"Unexpected value: found $this, expected $tp")

    def asInt: Int            = expected("Int")
    def asString: String      = expected("String")
    def asBool: Boolean       = expected("Boolean")
    def asObject: ObjectValue = expected("Object")
    def asArray: ArrayValue   = expected("Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal(s"Unknown field '$name'")
      }
    }

    def getField(name: String) = {
      fields.get(name) match {
        case Some(Some(v)) => v
        case Some(None) => fatal(s"Field '$name' has not been initialized")
        case None => fatal(s"Unknown field '$name'")
      }
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(entries: Array[Int]) extends Value {
    val length = entries.length

    private def checkBounds(index: Int) = {
      if (index < 0 || index >= length) {
        fatal(s"Index '$index' out of bounds (0 .. ${length-1})")
      }
    }

    def setIndex(i: Int, v: Int) {
      checkBounds(i)
      entries(i) = v
    }

    def getIndex(i: Int) = {
      checkBounds(i)
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(v: String) extends Value {
    override def asString = v
  }

  case class IntValue(v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(v: Boolean) extends Value {
    override def asBool = v
  }
}
