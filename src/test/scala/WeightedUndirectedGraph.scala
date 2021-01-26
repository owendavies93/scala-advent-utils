package scalaadventutils

import org.scalatest.funsuite.AnyFunSuite

class WeightedUndirectedGraphSpec extends AnyFunSuite {

    test("Basic string graph") {
        val g = Map("a" -> Map("b" -> 464, "c" -> 518),
                    "b" -> Map("d" -> 10, "c" -> 12),
                    "c" -> Map("e" -> 2),
                    "e" -> Map("f" -> 1))

        val graph = new WeightedUndirectedGraph(g)
        graph.getAllPaths("a")

        val g2 = Map("l" -> Map("d" -> 464, "b" -> 518),
                     "d" -> Map("b" -> 141))

        val graph2 = new WeightedUndirectedGraph(g2)
        graph2.getAllPaths("l")

        assertResult(Set("a")) {
            graph.getRootNodes
        }

        assertResult(Set("l")) {
            graph2.getRootNodes
        }

        val g3 = Map(
            0 -> Map(2 -> 1),
            5 -> Map(6 -> 1),
            1 -> Map(1 -> 1),
            6 -> Map(4 -> 1, 5 -> 1),
            2 -> Map(0 -> 1, 3 -> 1, 4 -> 1),
            3 -> Map(2 -> 1, 4 -> 1),
            4 -> Map(2 -> 1, 3 -> 1, 6 -> 1)
        )

        val graph3 = new WeightedUndirectedGraph(g3)
        graph3.getAllPaths(0)
    }
}
