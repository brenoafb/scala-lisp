package lisp

trait Expr

case class Atom(s: String) extends Expr

case class PList(xs: List[Expr]) extends Expr

def expr: Parser[Expr] = orP(atom, plist)

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

@main def main() =
  println(expr.parse("(hello world)"))
