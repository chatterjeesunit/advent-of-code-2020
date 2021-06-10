import scala.collection.immutable.Queue

val l = List(1,2,3,4,5)
val q = Queue[Int]()
l.foldLeft(Queue[Int]()){(acc, n) => acc.enqueue(n)}

Queue(1,2,3,4,5).zipWithIndex