package fs2_guide
import cats.effect.{ExitCase, IO, Sync}
import fs2.Stream

import scala.annotation.tailrec

object EffectConstructor extends App {
  val error = new RuntimeException("some error")

  // EffectからStreamを作成できる
  println(Stream.eval(IO(1)).compile.toList.unsafeRunSync()) // => List(1)

  // Effectは失敗する可能性がある.失敗した場合には、Effectがcats.effect.IOのときには`unsafeRun~`系のメソッド実行時に例外が発生する.
  // これが`unsafeRun~`系のメソッドがunsafeだと言われる所以.
  try Stream.eval(IO.raiseError(error)).compile.toList.unsafeRunSync()
  catch {
    case e: Throwable => println(e)
  } // => java.lang.RuntimeException: some error

  // 独自型もcats.effect.Syncインスタンスを定義してあればcompileメソッドを呼べるようになる
  println(
    Stream.eval(LazyEither.right(1)).compile.toList.run()
  ) // => Right(List(1))
  println(
    Stream.eval(LazyEither.left(error)).compile.toList.run()
  ) // => Left(java.lang.RuntimeException: some error)

  // 複数の値から作成
  println(
    Stream.evals(LazyEither.right(List(1, 2))).compile.toList.run()
  ) // => Right(List(1, 2))
  println(
    Stream.evalSeq(LazyEither.right(Seq(1, 2))).compile.toList.run()
  ) // => Right(List(1, 2))

  // Effectを評価してstreamを作成するが、値を無視する.
  // そのため、toListなどするとF(List())のように空のListが出てくる.
  println(
    Stream.eval_(LazyEither.right(1)).compile.toList.run()
  ) // => Right(List())

  // Effectを評価し、成功・失敗をEitherで表した値を出力する.
  // そのため、Streamの実行自体は必ず成功する.
  println(
    Stream.attemptEval(LazyEither.right(1)).compile.toList.run()
  ) // => Right(List(Right(1)))
  println(
    Stream.attemptEval(LazyEither.left(error)).compile.toList.run()
  ) // => Right(List(Left(java.lang.RuntimeException: some error)))}

  // 繰り返しEffectを評価し、各繰り返しごとに値を生成する
  println(
    Stream.repeatEval(LazyEither.right(1)).take(5).compile.toList.run()
  ) // => Right(List(1, 1, 1, 1, 1))

  // 初期値と各イテレーションごとの値・次の値を実行するeffectfulな関数を渡すと繰り返しによってstreamが作成される.
  // 繰り返し実行時にNoneを返すとイテレーションがとまる.
  println(
    Stream
      .unfoldEval(1)(s =>
        LazyEither.right {
          s match {
            case 5 => None
            case _ => Some(s -> (s + 1))
          }
        }
      ).compile.toList.run()
  ) // => Right(List(1, 2, 3, 4))

  // unfoldEvalのように繰り返すが、インターフェースが微妙に違う
  println(
    Stream
      .unfoldLoopEval(1)(s =>
        LazyEither.right {
          (
            s,
            s match {
              case 5 => None
              case _ => Some(s + 1)
            }
          )
        }
      ).compile.toList.run()
  ) // => Right(List(1, 2, 3, 4, 5))
}

class LazyEither[L <: Throwable, R](val run: () => Either[L, R]) {
  def flatMap[B](a: R => LazyEither[L, B]): LazyEither[L, B] =
    LazyEither(run().flatMap(a(_).run()))

  def map[A](f: R => A): LazyEither[L, A] = LazyEither(run().map(f))

  def recover(f: Throwable => LazyEither[L, R]): LazyEither[L, R] =
    LazyEither(run().left.flatMap(f(_).run()))

  def attempt[B <: Throwable]: LazyEither[B, Either[L, R]] =
    LazyEither.right[B, Either[L, R]](run().fold(e => Left(e), a => Right(a)))
}

object LazyEither {
  def right[L <: Throwable, R](run: => R): LazyEither[L, R] =
    LazyEither(Right(run))

  def left[L <: Throwable, R](e: L): LazyEither[L, R] = LazyEither(Left(e))

  def apply[L <: Throwable, R](run: => Either[L, R]): LazyEither[L, R] =
    new LazyEither(() => run)

  def raise[L <: Throwable, R](e: L): LazyEither[L, R] = LazyEither.left(e)

  // Stream.compileメソッドのimplicitのために必要
  implicit def lazySync[L <: Throwable]: Sync[LazyEither[L, *]] =
    new Sync[LazyEither[L, *]] {
      override def suspend[A](thunk: => LazyEither[L, A]): LazyEither[L, A] =
        LazyEither(thunk.run())

      override def bracketCase[A, B](
        acquire: LazyEither[L, A]
      )(use: A => LazyEither[L, B])(
        release: (A, ExitCase[Throwable]) => LazyEither[L, Unit]
      ): LazyEither[L, B] = for {
        a        <- acquire
        resultOr <- use(a).attempt
        ec        = resultOr.fold(ExitCase.error, _ => ExitCase.Completed)
        _        <- release(a, ec)
        result   <- LazyEither(resultOr)
      } yield result

      override def flatMap[A, B](
        fa: LazyEither[L, A]
      )(f: A => LazyEither[L, B]): LazyEither[L, B] = fa.flatMap(f)

      @tailrec
      override def tailRecM[A, B](a: A)(
        f: A => LazyEither[L, Either[A, B]]
      ): LazyEither[L, B] = f(a).run() match {
        case Left(e)         => LazyEither.left(e)
        case Right(Left(b))  => tailRecM(b)(f)
        case Right(Right(b)) => LazyEither.right(b)
      }

      override def raiseError[A](e: Throwable): LazyEither[L, A] =
        LazyEither.raise(e.asInstanceOf[L])

      override def handleErrorWith[A](
        fa: LazyEither[L, A]
      )(f: Throwable => LazyEither[L, A]): LazyEither[L, A] = fa.recover(f)

      override def pure[A](x: A): LazyEither[L, A] = LazyEither.right(x)
    }
}
