package scalaadventutils

import de.ummels.prioritymap.PriorityMap
import scala.collection.mutable.Map
import scala.collection.mutable.Set

object Dijkstra {

    def dijkstra[N]
        ( graph:  WeightedUndirectedGraph[N]
        , source: N )
        : (Map[N, Int], Map[N, N]) = {

        val active = Set(source)
        var result = Map(source -> 0)
        var pred   = Map[N, N]()

        while (active.nonEmpty) {
            val minNode = active.minBy(result)
            active -= minNode

            val minCost = result(minNode)

            for ((node, cost) <- graph.get(minNode)){
                val cost_ = cost + minCost

                if (cost_ < result.getOrElse(node, Int.MaxValue)) {
                    active += node
                    result += (node -> cost_)
                    pred   += (node -> minNode)
                }
            }
        }

        (result, pred)
    }

    def dijkstraFast[N]
        ( graph: WeightedUndirectedGraph[N]
        , source: N )
        : (Map[N, Int], Map[N, N]) = {

        def find
            ( active: PriorityMap[N, Int]
            , res: Map[N, Int]
            , pred: Map[N, N])
            : (Map[N, Int], Map[N, N]) =
            if (active.isEmpty) (res, pred)
            else {
                val (minNode, minCost) = active.head

                val neighbours = for {
                    (node, cost) <- graph.get(minNode)
                        if !res.contains(node) &&
                           cost + minCost < active.getOrElse(node, Int.MaxValue)
                } yield node -> (cost + minCost)

                val preds = neighbours.mapValues(_ => minNode)

                find(active.tail ++ neighbours,
                     res + (minNode -> minCost),
                     pred ++ preds)
            }

        find(PriorityMap(source -> 0), Map.empty, Map.empty)
    }

    def shortestPath[N]
        ( graph: WeightedUndirectedGraph[N]
        , source: N
        , target: N
        , useFast: Boolean = false)
        : List[N] = {

        val pred = if (useFast) dijkstraFast(graph, source)._2
                   else dijkstra(graph, source)._2

        def right[N](x: N, pred: Map[N, N]): List[N] = {
            def go(x: N, acc: List[N]): List[N] = {
                pred.get(x) match {
                    case Some(y) => go(y, x :: acc)
                    case None    => x :: acc
                }
            }

            go(x, List.empty)
        }

        right(target, pred)
    }

    def shortestPathTotalWeight[N]
        ( g: WeightedUndirectedGraph[N]
        , path: List[N]) =
            path.sliding(2).map(edge => g.get(edge(0))(edge(1))).sum

    /*
        Geneartes a strongly connected tree, basically
        a pathological case, for benchmarking
    */
    def stronglyConnectedTree
        ( depth: Int)
        : WeightedUndirectedGraph[Int] = {

        val rnd   = new scala.util.Random
        val graph = (1 to depth).map(i =>
            i -> (1 to depth).filterNot(_ == i)
                             .map(_ -> (1 + rnd.nextInt(11))).toMap
        ).toMap

        new WeightedUndirectedGraph(graph)
    }
}
