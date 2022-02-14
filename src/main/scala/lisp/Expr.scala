package lisp

type Env = Map[String, Expr]

def mkIntOp(f: ((Int, Int) => Int)) =
  NativeFunc(xs =>
    xs match
      case List(x, y) =>
        (x, y) match
          case (Num(m), Num(n)) => Num(f(m,n))
          case _ => ??? // TODO report error
      case _ => ??? // TODO report error
  )



val defaultEnv: Env = Map(
  "+" -> mkIntOp(_+_),
  "-" -> mkIntOp(_-_),
  "*" -> mkIntOp(_*_),
  "/" -> mkIntOp(_/_)
)

trait Expr {
  def eval(env: Env): Expr = this
}

case class Num(n: Int) extends Expr {
  override def eval(env: Env) = this
}

case class Atom(s: String) extends Expr {
  override def eval(env: Env) = this
}

case class PList(xs: List[Expr]) extends Expr {
  def apply(env: Env, f: String, args: List[Expr]): Expr = {
    env.get(f) match
      case None =>
        println(s"Lookup error: $f")
        ???
      case Some(fExpr) =>
        apply(env, fExpr, args)
  }

  def apply(env: Env, f: Expr, args: List[Expr]): Expr = {
    val evaldArgs = args.map(_.eval(env))
    f match
      case NativeFunc(op) => op(evaldArgs)
      case _ => ??? // TODO report error
  }

  override def eval(env: Env) =
    xs match
      case Nil =>
        this
      case (Atom(f))::t =>
        apply(env, f, t)
      case _ =>
        this
}

case class NativeFunc(func: (List[Expr] => Expr)) extends Expr {
}

def expr: Parser[Expr] = orP(num, orP(atom, plist))

def plist: Parser[Expr] = for {
  _ <- char('(')
  xs <- many(before(expr, many(whitespace)))
  _ <- char(')')
} yield PList(xs)

def symbol: Parser[String] =
  oneOfString("_<>=+-*&^%$#@!").map(_.toString)

def atom: Parser[Expr] =
  val firstLetter: Parser[Char] =
    orP(singleP(_.isLetter), oneOfString("=+-_*&^%$#@!"))

  val remaining: Parser[Char] =
      orP(firstLetter, singleP(_.isDigit))

  val p = for {
    f <- firstLetter
    r <- many(remaining)
  } yield f + r.mkString

  p.map(Atom(_))

def num: Parser[Expr] =
  int.map(Num(_))
