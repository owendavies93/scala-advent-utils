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

    test("CircularList: rotate") {
        val cl = CircularList(4)(1, 2, 3)

        val cl2 = cl.rotate(1)

        assert(cl2.size == cl.size)

        assertResult(CircularList(4)(2, 3, 1)) {
            cl2
        }

        val cl3 = cl.rotate(-2)

        assertResult(CircularList(4)(2, 3, 1)) {
            cl3
        }
    }

    test("CircularList: insertAt") {
        val cl = CircularList(4)(1, 2, 3)

        val cl2 = cl.insertAt(0, 10)

        assert(cl2.size == 4)

        assertResult(CircularList(4)(10, 1, 2, 3)) {
            cl2
        }

        val cl3 = cl.insertAt(3, 10)

        assert(cl3.size == 4)

        assertResult(CircularList(4)(1, 2, 3, 10)) {
            cl3
        }

        val cl4 = cl.insertAt(4, 10)

        assert(cl4.size == 3)

        val cl5 = cl3.insertAt(0, 3)

        assert(cl5.size == 4)

        assertResult(CircularList(4)(1, 2, 3, 10)) {
            cl5
        }
    }

    test("CircularList: removeAt") {
        val cl = CircularList(4)(1, 2, 3)

        val (e, cl2) = cl.removeAt(2)

        assert(e == 3)

        assertResult(CircularList(4)(1, 2)) {
            cl2
        }

        val (e1, cl3) = cl.removeAt(4)

        assert(e1 == 2)

        assertResult(CircularList(4)(1, 3)) {
            cl3
        }

        val (e2, cl4) = cl.removeAt(-2)

        assert(e1 == 2)

        assertResult(CircularList(4)(1, 3)) {
            cl4
        }

        val cl5 = CircularList(6)(1,2,3,4,5,6)

        val (e3, cl6) = cl5.removeAt(3)

        assert(e3 == 4)

        assertResult(CircularList(6)(1,2,3,5,6)) {
            cl6
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

