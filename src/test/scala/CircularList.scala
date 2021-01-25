package scalaadventutils

import scala.collection.immutable.Queue

import org.scalatest.funsuite.AnyFunSuite

class CircularListSpec extends AnyFunSuite {

    test("CircularList: push") {
        val cl = CircularList(4)(1, 2, 3)

        assert(cl.size == 3)

        val (_, cl2) = cl.push(3)

        assert(cl2.size == 4)

        val (head, cl3) = cl2.push(4)

        assert(cl3.size == 4)

        assertResult(Some(1)) {
            head
        }
    }

    test("CircularList: pop") {
        val cl = CircularList(4)(1, 2, 3)

        val (e, cl2) = cl.pop

        assert(cl2.size == 2)

        assertResult(1) {
            e
        }
    }

    test("CircularList: slice") {
        val cl = CircularList(4)(1, 2, 3, 4)

        val s = cl.slice(1, 3)

        assert(s.size == 2)

        assertResult(Queue(2, 3)) {
            s
        }

        val s2 = cl.slice(2, 5)

        assert(s2.size == 3)

        assertResult(Queue(3, 4, 1)) {
            s2
        }

        val c2 = CircularList(5)(4, 3, 0, 1, 2)

        val s3 = c2.slice(1, 6)

        assert(s3.size == 5)

        assertResult(Queue(3, 0, 1, 2, 4)) {
            s3
        }
    }

    test("CircularList: reverseSection") {
        val cl = CircularList(5)(0, 1, 2, 3, 4)

        var start = 0
        var length = 3
        val cl2 = cl.reverseSection(start, start + length)

        assertResult(CircularList(5)(2, 1, 0, 3, 4)) {
            cl2
        }

        start = 3
        length = 4
        val cl3 = cl2.reverseSection(start, start + length)

        assertResult(CircularList(5)(4, 3, 0, 1, 2)) {
            cl3
        }

        start = 1
        length = 5

        val cl4 = cl3.reverseSection(start, start + length)

        assertResult(CircularList(5)(3, 4, 2, 1, 0)) {
            cl4
        }
    }
}

