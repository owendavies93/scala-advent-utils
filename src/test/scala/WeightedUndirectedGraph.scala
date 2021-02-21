package scalaadventutils

import org.scalatest.funsuite.AnyFunSuite

class WeightedUndirectedGraphSpec extends AnyFunSuite {

    test("Basic string graph") {
        val g = Map("a" -> Map("b" -> 464, "c" -> 518),
                    "b" -> Map("d" -> 10, "c" -> 12),
                    "c" -> Map("e" -> 2),
                    "e" -> Map("f" -> 1))

        val graph = new WeightedUndirectedGraph(g)

        assertResult(List(
            List("a", "b", "d"),
            List("a", "b", "c", "e", "f"),
            List("a", "c", "e", "f")
        )) {
            graph.getAllPaths("a")
        }

        val g2 = Map("l" -> Map("d" -> 464, "b" -> 518),
                     "d" -> Map("b" -> 141))

        val graph2 = new WeightedUndirectedGraph(g2)

        assertResult(List(List("l", "d", "b"), List("l", "b"))) {
            graph2.getAllPaths("l")
        }

        assertResult(Set("a")) {
            graph.getRootNodes
        }

        assertResult(Set("l")) {
            graph2.getRootNodes
        }

        val g3 = Map(
            0 -> Map(2 -> 1),
            5 -> Map(6 -> 1),
            1 -> Map[Int, Int](),
            6 -> Map(4 -> 1, 5 -> 1),
            2 -> Map(0 -> 1, 3 -> 1, 4 -> 1),
            3 -> Map(2 -> 1, 4 -> 1),
            4 -> Map(2 -> 1, 3 -> 1, 6 -> 1)
        )

        val graph3 = new WeightedUndirectedGraph(g3)

        assertResult(Set(Set(0,2,3,4,5,6), Set(1))) {
            graph3.getAllConnectedComponents
        }

        assertResult(2) {
            graph3.countConnectedComponents
        }

        val g4 = Map(
            1937 -> Map(249 -> 1),
            249  -> Map(1937 -> 1)
        )

        val graph4 = new WeightedUndirectedGraph(g4)

        assertResult(1) {
            graph4.countConnectedComponents
        }

        val graph5 = graph.removeEdgesTo("c")

        assertResult(Map("b" -> 464)) {
            graph5.get("a")
        }
    }
}
