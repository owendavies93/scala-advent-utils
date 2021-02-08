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

    def insertAt(i: Int, e: T) =
        if (size == capacity || i > size) this
        else CircularList(
            capacity, size + 1, slice(0, i).enqueue(e) ++ slice(i, size))

    def removeAt(i: Int): (T, CircularList[T]) = {
        val modIndex = if (i >= 0) i % size else i + size
        val (e, l) = rotate(modIndex).pop
        (e, l.rotate(l.size - modIndex))
    }

    def rotate(times: Int = 1): CircularList[T] = {
        val t = if (times < 0) size + times else times
        (1 to t).foldLeft(this)((cl, _) => {
            val (elem, cl_) = cl.pop
            cl_.push(elem)._2
        })
    }

    def reverseSection(from: Int, to: Int): CircularList[T] =
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
        else if (from >= size)
            queue.slice(from % size, to % size)
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
