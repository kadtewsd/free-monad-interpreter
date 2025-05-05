import cats.{Id, ~>}
import cats.free.Free

import scala.io.StdIn

class Test {

  // typealias を使わずに ~> を表現する書き方
  val interpreterByFunctionK = new cats.arrow.FunctionK[ConsoleDSL, Id] {
    def apply[A](fa: ConsoleDSL[A]) : Id[A] = fa match {
      case WriteLog(msg) => msg match {
        case ProdLog(msg) => throw new IllegalCallerException("テスト用ではコールできません")
        case TestLog(msg) => println(s"[Test] テストです。$msg")
      }
      case ReadLine => StdIn.readLine()
    }
  }

  // FunctionK を ~> で表現した書き方
  val interpreter = new ~>[ConsoleDSL, Id] {
    // ConsoleDSL の命令（パターン）に応じた処理を書く
    def apply[A](fa: ConsoleDSL[A]) : Id[A] = fa match {
      case WriteLog(msg) => msg match {
        case ProdLog(msg) => throw new IllegalCallerException("テスト用ではコールできません")
        case TestLog(msg) => println(s"[Test] テストです。$msg")
      }
      case ReadLine => StdIn.readLine()
    }
  }

  /**
   * Scala では型引数が2つある型（例: F ~> G）のインスタンスを new で作る際に、型を括弧で囲むことができます。
   * これは中置型エイリアス（この場合 `~>`）を明示的にインスタンス化する際によく使われる表記です。
   *
   * `~>` は Cats で定義された型エイリアスであり、`ConsoleDSL ~> Id` は `FunctionK[ConsoleDSL, Id]` を意味します。
   * この `FunctionK` は高カインド型 F[_], G[_] に対する型レベルの関数を表し、ADT (代数的データ型) のインタプリタとして利用されます。
   *
   * def apply[A](fa: ConsoleDSL[A]): Id[A] の定義により、ConsoleDSL に対して A の型ごとに振る舞いを定義することが可能になります。
   * これは OOP におけるメソッドのオーバーライドのような役割を果たし、データと振る舞いの分離によって関数型らしい設計が可能になります。
   *
   * Kotlin でも `ConsoleDSL<*>` のような表現は可能ですが、型パラメータを網羅的に扱う手段が乏しいため、
   * 型消去や unsafe cast に頼らざるを得ないことがあります。
   * Scala は高カインド型とパターンマッチングを組み合わせることで、安全に型ごとの振る舞いを実装できるのが特徴です。
   */
  private val testInterpreter = new (ConsoleDSL ~> Id) {
    def apply[A](fa: ConsoleDSL[A]) : Id[A] = fa match {
      case WriteLog(msg) => msg match {
        case ProdLog(msg) => throw new IllegalCallerException("テスト用ではコールできません")
        case TestLog(msg) => println(s"[Test] テストです。$msg")
      }
      case ReadLine => StdIn.readLine()
    }
  }

  def execute(): Unit = {
    //
    /**
     * Free の一つ目の型は、F[_] になっている。
     * F[_] は、「型引数を一つ取る型コンストラクタ」Option[_], List[_], といったもの。
     * これらはモナドの要件である、map/flatMap を持っているものになっている
     * liftF でオブジェクトをモナドに持ち上げている。
     * def liftF[F[_], A](value: F[A]): Free[F, A] = Suspend(value)
     * になっているが、F[_] と一度定義した後、Free[F, A] と書くと、F は、型コンストラクタとして扱われるので実質 F[_] と同じ。
     * なぜ、Free[F[A]] ではなく、Free[F, A]  で 書いているかというと、変換器 (testInterpreter) にて、パターンマッチする時、ConsoleDSL[A] にしてしまうと、型が確定してしまっているので、パターンマッチをできない。
     * まだなにが型になるのかわからないので、F[_] で定義しています。たぶん、変換器のために、F[_] があると思われます。
     *
     */
    val program: Free[ConsoleDSL, Unit] = for {
      _  <- ConsoleDSL.writeLog(TestLog("お名前を聞かせてください"))
      name <- ConsoleDSL.readLine()
      _ <- ConsoleDSL.writeLog(TestLog(s"こんにちは $name さん"))
    } yield ()

    program.foldMap(testInterpreter)
  }
}
