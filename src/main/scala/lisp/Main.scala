package lisp

import scala.io.StdIn.readLine

def loop(): Unit = {
  val input = readLine()
  expr.parse(input) match
    case Some((e,"")) =>
      println(e)
      println(s"result: ${e.eval(defaultEnv)}")
    case _ => println("error")
  loop()
}

@main def main(): Unit = {
  loop()
}
