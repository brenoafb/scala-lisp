package lisp

trait Functor[F[_]]:
  extension [A](x: F[A])
    def map[B](f: A => B): F[B]

trait Monad[F[_]] extends Functor[F]:
  def pure[A](x: A): F[A]
  extension [A](x: F[A])
    def flatMap[B](f: A => F[B]): F[B]
    def map[B](f: A => B) =
      x.flatMap(f.andThen(pure))

trait Parser[A] {
  def parse: String => Option[(A, String)]
}

given Functor[Parser] with
  extension [A](p: Parser[A])
    def map[B](f: A => B): Parser[B] =
      new {
        def parse = (s: String) => for {
          (x, s1) <- p.parse(s)
        } yield (f(x), s1)
      }

given Monad[Parser] with

  def pure[A](x: A): Parser[A] =
    new {
      def parse = (s: String) => Some(x, s)
    }

  extension [A](p: Parser[A])
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      new {
        def parse = (s: String) => for {
          (x, s1) <- p.parse(s)
          (y, s2) <- f(x).parse(s1)
        } yield (y, s2)
      }

def char(c: Char): Parser[Char] =
  val f = (s: String) =>
    if s.startsWith(c.toString)
    then
      val (_, s1) = s.splitAt(1)
      Some((c, s1))
    else None
  new {
    def parse = f
  }

def sequenceParser[A](xs: List[Parser[A]]): Parser[List[A]] = {
  xs match
  case mh::mt => for {
    h: A <- mh
    t: List[A] <- sequenceParser(mt)
  } yield h :: t
  case Nil  => pure(Nil)
}

def pure[A](x: A): Parser[A] =
  new {
    def parse = (s: String) => Some(x, s)
  }

def span(f: (Char => Boolean), s: String): (String, String) =
  (s.takeWhile(f), s.dropWhile(f))

def span(f: Char => Boolean): Parser[String] =
  new {
    def parse = (s: String) =>
      val (s1, s2) = span(f, s)
      s1.length match
      case 0 => None
      case _ => Some((s1, s2))
  }

def singleP(f: Char => Boolean): Parser[Char] =
  new {
    def parse = (s: String) =>
    s.toList match
      case Nil => None
      case x :: xs =>
        if f(x) then Some (x, xs.mkString) else None
  }

def int: Parser[Int] =
  span(_.isDigit).map(_.toInt)

def word: Parser[String] =
  span(_.isLetterOrDigit)

def whitespace: Parser[String] =
  span(_.isWhitespace)

def ident(id: String): Parser[String] =
  val listOfCharParser: List[Parser[Char]]= id.map(char(_)).toList
  val p: Parser[List[Char]] = sequenceParser(listOfCharParser)
  p.map(_.mkString)

def orP[A](p: Parser[A], q: Parser[A]): Parser[A] =
  new {
    def parse = (s: String) =>
    p.parse(s) match
    case Some(x,y) => Some(x,y)
    case None =>
      q.parse(s) match
        case Some(x,y) => Some(x,y)
        case None => None
  }

def many[A](p: Parser[A]): Parser[List[A]] =
  orP(some(p), pure(Nil))

def some[A](p: Parser[A]): Parser[List[A]] = for {
  x <- p
  xs <- many(p)
} yield x :: xs

def before[A,B](p: Parser[A], s: Parser[B]): Parser[A] = for {
  x <- p
  _ <- s
} yield x

def paren[A](p: Parser[A]): Parser[A] = for {
  _ <- char('(')
  _ <- many(whitespace)
  x <- p
  _ <- many(whitespace)
  _ <- char(')')
} yield x

def helloWorld = for {
  hello <- ident("hello")
  w <- whitespace
  world <- ident("world")
} yield ()

def none[A]: Parser[A] =
  new {
    def parse = (s: String) => None
  }

def oneOf(xs: List[Char]): Parser[Char] =
  xs match
    case Nil => none
    case c :: cs =>
      val accumulator: ((Char, Parser[Char]) => Parser[Char]) =
        (ch: Char, acc: Parser[Char]) => orP(acc, char(ch))
      cs.foldRight(char(c))(accumulator)

def oneOfString(s: String): Parser[Char] =
  oneOf(s.toList)
