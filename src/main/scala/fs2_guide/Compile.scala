package fs2_guide

import cats.effect.IO
import fs2.{Pure, Stream}

/** Streamに生えているcompileメソッドの意味論について解説する */
object Compile extends App {
  // 1. algebrasについて
  // FPではOptionの様な型をalgebrasと呼ぶ
  // そして値をalgebrasの中に入れ込むことを`lifting`と呼ぶ
  val opt: Option[Int] = Option(1) // => IntがOption algebrasの世界にliftされた

  // 2. algebrasの世界でプログラムを組み立てる
  // あるalgebrasの中にliftしたあとは、algebrasの世界でプログラムを組み立てることができる
  val add1: Option[Int]         = opt.map(_ + 1) // mapはalgebrasの中で値を変換する
  val composeAlges: Option[Int] =
    opt.flatMap(i => add1.map(i + _)) // flatMapは複数のalgebrasを合成する
  // この様にalgebrasにプログラムを組み合わせる関数をcombinatorと呼ぶ
  // mapやflatMapの他にも様々なcombinatorが存在する
  // それらを組み合わせることで小さなプログラムからalgebrasの世界で大きなプログラムを組み立てる事ができる

  // for式を使うとよりalgebrasの世界でプログラムを組み立てていることが強調される
  // for式の中はOption algebrasの中での計算を表す
  val calculated: Option[Int] = for {
    value <- opt
    add1   = value + 1
    other <- Option(2)
  } yield add1 + other

  // 3. algebrasの翻訳
  // algebrasはプリミティブな値ではないので最終的にはIntやStringなどのプリミティブな型に翻訳(transform)する必要がある
  // 翻訳することをFPでは`run`と呼んだりする
  // 例えばOption algebrasはgetOrElseやfoldを使用してプリミティブ値に翻訳することができる
  val intOrZero: Int       = opt.getOrElse(0)
  val intOrZeroStr: String = calculated.fold("0")(_.toString)
  // この様にプリミティブ値に変換することで最終的にプログラムに必要な値を取得することができる

  // algebrasはプリミティブだけでなく他のalgebrasに翻訳することもできる
  // 例えばIO algebrasへの変換
  val io: IO[Int] =
    opt.fold(IO.raiseError[Int](new RuntimeException("empty")))(IO(_))

  // 4. algebrasとしてのStream
  // fs2.Streamもalgebrasとみなすことができる
  val stream: Stream[IO, Int]   = Stream.evalSeq(IO(Seq(1, 2, 3)))
  val composed: Stream[IO, Int] = for {
    value <- stream
    other <- Stream.eval(IO(4))
  } yield value + other

  // algebrasであるからには翻訳することができる
  // Stream[IO, Int]のalgebrasからはIO[Int]algebrasに翻訳することができる
  // 翻訳の際に使用されるのがcompileメソッド
  // compileはCompiler型クラスによって保証される
  val ioCompiler: Stream.CompileOps[IO, IO, Int] = composed.compile
  // CompilerOpsはStreamの翻訳を担当するデータ型
  // 例えばこのデータ型を使用することでIO[List[Int]]の変換ができる
  val ioList: IO[List[Int]]                      = ioCompiler.toList
  // 他にもVectorの翻訳などがサポートされている
  val ioVector: IO[Vector[Int]]                  = ioCompiler.toVector

  // Compilerは翻訳のための様々なメソッドを提供している
  // 例えばfoldメソッドを使用することでIOの様々なalgebrasに翻訳できる
  val ioSum: IO[Int] =
    ioCompiler.fold(0)(_ + _) // Stream[IO, Int]がIO[Int]に翻訳された
  // Streamを実行したいが、最終的な値がいらないときはdrainが使用できる
  val ioUnit: IO[Unit] = ioCompiler.drain // IO[Unit]に翻訳された

  // そしてIOの場合は最終的にプリミティブに翻訳してプログラムを実行する
  println(ioSum.unsafeRunSync()) // => 18

  // 5. Pure型を使用した場合のStreamの翻訳
  // StreamはEffectを使用しないで構成することもできる
  val pureStream: Stream[Pure, Int] = Stream(1, 2, 3)
  // この場合PureというEffectがないことを示す型がプレースホルダー的に使用される
  // pureなStreamの場合はStream[F[_], A]なStreamをF[_] algebrasへの翻訳が必要ないので直接Listなどのalgebrasに翻訳できる
  // この場合はCompilerは必要ない
  println(pureStream.toList) // => List(1, 2, 3)

  // 6. まとめ
  // - algebrasはFPでよく使用されるterm/テクニック
  // - algebrasには翻訳という概念がある
  // - fs2.Streamも翻訳の一種
  // - StreamはCompilerを使用することで他のalgebrasに翻訳することができる
  // - pureなStreamの場合はF[_] algebrasへの翻訳を介さないで直接Listなどのalgebrasに翻訳できる
}
