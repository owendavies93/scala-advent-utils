package scalaadventutils

import scala.collection.immutable.Queue

case class CircularList[T](capacity: Int, size: Int, queue: Queue[T]) {

    def push(e: T): (Option[T], CircularList[T]) =
        if (size < capacity)
            (None, CircularList(capacity, size + 1, queue.enqueue(e)))
        else queue.dequeue match {
            case (head, q) =>
                (Some(head), CircularList(capacity, size, q.enqueue(e)))
        }

    def pop: (T, CircularList[T]) = queue.dequeueOption.map {
        case (head, q) => (head, CircularList(capacity, size - 1, q))
    }.getOrElse(throw new NoSuchElementException)

    def reverseSection(from: Int, to: Int) =
        if (from >= to) CircularList(capacity, size, queue)
        else {
            val section = slice(from, to).reverse
            if (to > size) {
                val (end, start) = section.splitAt(size - from)

                CircularList(capacity, size,
                    start ++ queue.slice(to % size, from) ++ end)
            } else {
                CircularList(capacity, size,
                    queue.slice(0, from) ++ section ++ queue.slice(to, size))
            }
        }

    def slice(from: Int, to: Int): Queue[T] = {
        if (from >= to) Queue.empty
        else if (to >= size)
            queue.slice(from, size) ++ queue.slice(0, to % size)
        else
            queue.slice(from, to)
    }

}

object CircularList {

    def apply[T](lim: Int)(elems: T*): CircularList[T] = {
        val ts = if (elems.size <= lim) elems else elems.takeRight(lim)
        CircularList(lim, ts.size, Queue(ts: _*))
    }
}
