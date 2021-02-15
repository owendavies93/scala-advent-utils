package scalaadventutils

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class WeightedUndirectedGraph[N](graph: Map[N, Map[N, Int]]) {

    def get(n: N) = graph.getOrElse(n, Map.empty)

    def keys = graph.keys

    def neighbours(n: N) = get(n).keys

    def countConnectedComponents = {
        @tailrec
        def count(visited: Set[N], c: Int): Int = {
            if (visited == keys.toSet) c
            else {
                val unseen = keys.toSet diff visited
                val com = getConnectedComponent(unseen.head)
                count(visited ++ com, c + 1)
            }
        }
        count(Set[N](), 0)
    }

    def getAllConnectedComponents = {
        @tailrec
        def get(visited: Set[N], components: List[Set[N]]): List[Set[N]] = {
            if (visited == keys.toSet) components
            else {
                val unseen = keys.toSet diff visited
                val com = getConnectedComponent(unseen.head)
                get(visited ++ com, components :+ com)
            }
        }
        get(Set[N](), List[Set[N]]())
    }

    def getAllPaths(start: N): List[List[N]] = {
        var paths = new ListBuffer[List[N]]()

        def pathFinder
            (node: N
            , paths: ListBuffer[List[N]]
            , currentPath: ListBuffer[N]
            , visited: collection.mutable.Set[N]): Unit = {

            val ns = neighbours(node)
            if (ns.isEmpty || ns.forall(visited.contains(_))) {
                paths += currentPath.toList
                return
            }

            visited += node

            for (n <- ns) {
                if (!visited.contains(n)) {
                    currentPath += n
                    pathFinder(n, paths, currentPath, visited)
                    currentPath -= n
                }
            }

            visited -= node
        }
/*
        def pathFinder
            ( node: N
            , paths: List[List[N]]
            , currentPath: List[N]
            , visited: Set[N])
            : List[List[N]] = {

            val ns = neighbours(node)

            if (ns.isEmpty || ns.forall(visited.contains(_)))
                paths :+ currentPath
            else {
                ns.filterNot(n => (visited + n).contains(n)).map(n =>
                    pathFinder(n, paths, currentPath :+ n, visited + n)
                )
            }
        }
*/
        pathFinder(
            start, paths, ListBuffer[N](start), collection.mutable.Set[N]()
        )
        paths.toList
    }

    def getConnectedComponent(start: N) =
        getAllPaths(start).flatten.distinct.toSet

    def getRootNodes = graph.keys.toSet diff graph.values.flatMap(_.keys).toSet

    def removeEdgesTo(node: N) = new WeightedUndirectedGraph(
        keys.map(k =>
            k -> graph(k).-(node)
        ).toMap
    )
}
