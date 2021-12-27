package edu.colorado.csci3155s2021.project2

object Interpreter {
    /*-- Helper functions --*/
    def mkLine(l: Expr, env: Environment) =
        {
            val v1 = evalExpr(l, env)
            v1 match {
                case NumValue(len) => {
                    val ln = new Polygon(List((0, 0), (len, 0)))
                    val f = new MyCanvas(List(ln))
                    FigValue(f)
                }
                case _ => throw new IllegalArgumentException("Cannot create line with non numerical length")
            }
        }

    def mkTriangle(expr: Expr, environment: Environment): Value = {
        val v1 = evalExpr(expr, environment)
        v1 match {
            case NumValue(len) => {
                val tri = new Polygon(List((0,0), (len,0), (len/2.0, math.sqrt(3.0)*len/2.0)))
                val f = new MyCanvas(List(tri))
                FigValue(f)
            }
            case _ => throw new IllegalArgumentException("Cannot create triangle with non numerical length")
        }
    }

    def mkRectangle(sideLength: Expr, env: Environment): Value = {
        val v1 = evalExpr(sideLength, env)
        v1 match {
            case NumValue(len) => {
                val sq = new Polygon(List((0,0), (len,0), (len,len), (0,len)))
                val f = new MyCanvas(List(sq))
                FigValue(f)
            }

            case _ => throw new IllegalArgumentException("Cannot create rectangle with non numerical length")
        }
    }

    def mkCircle(rad: Expr, env: Environment): Value = {
        val v1 = evalExpr(rad, env)
        v1 match {
            case NumValue(rad) => {
                val sq = new MyCircle((rad, rad), rad)
                val f = new MyCanvas(List(sq))
                FigValue(f)
            }
            case _ => throw new IllegalArgumentException("Cannot create rectangle with non numerical length")
        }
    }

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case Ident(s) => env.lookup(s)
        case Line(l) => mkLine(l, env) //TODO: Handle a line object. Note that l is an expression it needs to be evaluated first.
        case EquiTriangle(sideLength) => mkTriangle(sideLength, env) //TODO: Handle a triangle object.Note that sideLength is an expression it needs to be evaluated first.
        case Rectangle(sideLength) => mkRectangle(sideLength, env) //TODO: Handle a rectangle object.Note that sideLength is an expression it needs to be evaluated first.
        case Circle(rad) => mkCircle(rad, env) //TODO: Handle a circle object
        case Plus (e1, e2) => { //TODO: Handle Plus
            (evalExpr(e1, env), evalExpr(e2, env)) match {
                case (NumValue(a), NumValue(b)) => NumValue(a+b)
                case (FigValue(a), FigValue(b)) => FigValue(a.overlap(b))
                case _ => throw new IllegalArgumentException("Invalid operands")
            }
        }
        case Minus (e1, e2) => { //TODO: Handle Minus
            (evalExpr(e1, env), evalExpr(e2, env)) match {
                case (NumValue(a), NumValue(b)) => NumValue(a-b)
                case _ => throw new IllegalArgumentException("Invalid operands")
            }
        }

        case Mult(e1, e2) => { // TODO: Handle Multiplication
            (evalExpr(e1, env), evalExpr(e2, env)) match {
                case (NumValue(a), NumValue(b)) => NumValue(a * b)
                case (FigValue(a), FigValue(b)) => FigValue(a.placeRight(b))
                case _ => throw new IllegalArgumentException("Invalid operands")
            }
        }
        case Div(e1, e2) => { // TODO: Handle Division
            (evalExpr(e1, env), evalExpr(e2, env)) match {
                case (NumValue(a), NumValue(b)) => NumValue(a / b)
                case (FigValue(a), FigValue(b)) => FigValue(b.placeTop(a))
                case (FigValue(a), NumValue(b)) => {
                    val (_, xmax, ymin, _) = a.getBoundingBox
                    FigValue(a.translate(-xmax, -ymin).rotate(b).translate(xmax,ymin))
                }
                case _ => throw new IllegalArgumentException("Invalid operands")
            }
        }
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => Closure(x, e, env) //TODO: Handle FunDef -- look at Value.scala
        case LetRec(f, x, e1, e2) =>  evalExpr(e2, ExtendREC(f,x,e1,env)) // TODO: Handle recursive functions -- look at Environment.scala
        case FunCall(fCall, arg) => { // TODO: Handle function calls
            evalExpr(fCall, env) match {
                case Closure(x, closure, envb) => evalExpr(closure, Extend(x, evalExpr(arg, env), envb))
                case _ => throw new IllegalArgumentException("Invalid operands")
            }
        }
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
