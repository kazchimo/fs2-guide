package fs2_guide
import fs2.Stream

/** 単純な値からStreamを作成する方法を解説する */
object SimpleConstructor extends App {
  // 空Streamの作成
  println(Stream.empty.toList) // => List()

  // ひとつの値からStreamを作成できる
  println(Stream.emit(1).toList) // => List(1)

  // 複数の値からStreamの作成
  println(Stream.emits(List(1, 2)).toList)    // => List(1, 2)
  println(Stream(1, 2).toList)                // => List(1, 2)
  println(Stream.iterable(List(1, 2)).toList) // => List(1, 2)
}
