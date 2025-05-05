// src/main/scala/Main.scala
import cats.free.Free
import cats.free.Free.liftF
import cats.{~>, Id}
import scala.io.StdIn

// DSLの定義
sealed trait Log
case class ProdLog(msg: String) extends Log
case class TestLog(msg: String) extends Log
case object ReadLine extends ConsoleDSL[String]

// DSLを持ち上げる関数
sealed trait ConsoleDSL[A]
case class WriteLog(msg: Log) extends ConsoleDSL[Unit]
case class ReadLine() extends ConsoleDSL[String]
// Free モナドで DSL を包む
object ConsoleDSL {
  def writeLog(log: Log): Free[ConsoleDSL, Unit] = liftF(WriteLog(log))
  def readLine(): Free[ConsoleDSL, String] = liftF(ReadLine)
}

// 実行インタプリタ
object ProdInterpreter extends (ConsoleDSL ~> Id) {
  def apply[A](fa: ConsoleDSL[A]): Id[A] = fa match {
    case WriteLog(msg) => msg match {
      case ProdLog(msg) => println(s"[prod] $msg")
      case TestLog(msg) => throw new IllegalArgumentException("type mismatched")
    }
    case ReadLine =>
      StdIn.readLine()
  }
}

// サンプルプログラム
object Main extends App {
  import ConsoleDSL._

  val program: Free[ConsoleDSL, Unit] = for {
    _    <- writeLog(ProdLog("お名前は？"))
    name <- readLine()
    _    <- writeLog(ProdLog(s"こんにちは、$name さん！"))
  } yield ()
  program.foldMap(ProdInterpreter) // 実行
}