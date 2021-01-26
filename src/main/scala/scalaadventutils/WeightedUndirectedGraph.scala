package scalaadventutils

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class WeightedUndirectedGraph[N](graph: Map[N, Map[N, Int]]) {

    def get(n: N) = graph.getOrElse(n, Map.empty)

    def keys = graph.keys

    def neighbours(n: N) = get(n).keys

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
            , visited: collection.mutable.Set[N]) {

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

        var p = ListBuffer[N](start)
        var v = collection.mutable.Set[N]()
        pathFinder(start, paths, p, v)
        paths.toList
    }

    def getConnectedComponent(start: N) =
        getAllPaths(start).flatten.distinct.toSet

    def getRootNodes = graph.keys.toSet diff graph.values.flatMap(_.keys).toSet
}
