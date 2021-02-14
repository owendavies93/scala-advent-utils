package scalaadventutils

import org.scalatest.funsuite.AnyFunSuite

class DijkstraSpec extends AnyFunSuite {

    test("Basic string graph") {
        val g = Map("a" -> Map("b" -> 464, "c" -> 518),
                    "b" -> Map("c" -> 141, "d" -> 10),
                    "c" -> Map("a" -> 2))

        val graph = new WeightedUndirectedGraph[String](g)

        assertResult(474) {
            val path = Dijkstra.shortestPath(graph, "a", "d")
            Dijkstra.shortestPathTotalWeight(graph, path)
        }

        assertResult(474) {
            val path = Dijkstra.shortestPath(graph, "a", "d", true)
            Dijkstra.shortestPathTotalWeight(graph, path)
        }
    }

    test("Strongly connected tree") {
        val tree = Dijkstra.stronglyConnectedTree(100)
        Profiler.timeMicS { Dijkstra.dijkstra(tree, 1) }
        Profiler.timeMicS { Dijkstra.dijkstraFast(tree, 1) }
    }
}
