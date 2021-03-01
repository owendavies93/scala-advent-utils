package scalaadventutils

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait NodeSet[N] {
    // Unweighted distances (nodes between)
    def distances: Map[N, Int]

    def nodes: Set[N] = distances.keySet
}

trait To[N] {
    def to: Option[(N, Int)]
}

class WeightedUndirectedGraph[N](graph: Map[N, Map[N, Int]]) {

    def get(n: N) = graph.getOrElse(n, Map.empty)

    def keys = graph.keys

    def neighbours(n: N) = get(n).keys

    def countConnectedComponents: Int = getAllConnectedComponents.size

    def getAllConnectedComponents: Set[Set[N]] = {
        val nodeSet = keys.toSet

        def getComp(nodes: Set[N]): Set[Set[N]] = {
            if (nodes.isEmpty) Set.empty
            else {
                val start = nodes.head
                val component = getConnectedComponent(start)
                val remaining = nodes -- component
                getComp(remaining) + component
            }
        }
        getComp(nodeSet)
    }

    def getConnectedComponent(start: N) = traverseFrom(start).nodes

    def traverseFrom(start: N): NodeSet[N] = {

        @tailrec
        def traverse(v: Map[N, Int], toVisit: Map[N, Int]): NodeSet[N] = {
            val neighs = for {
                (n, d) <- toVisit
                neigh  <- neighbours(n)
            } yield neigh -> (d + 1)

            val nextVisited = v ++ toVisit
            val nextToVisit = neighs -- v.keys

            if (nextToVisit.isEmpty)
                new NodeSet[N] { override def distances = nextVisited }
            else
                traverse(nextVisited, nextToVisit)
        }

        traverse(Map.empty, Map(start -> 0))
    }

    def searchFrom(start: N, toF: (N => Boolean)): NodeSet[N] with To[N] = {

        @tailrec
        def search(v: Map[N, Int], tv: Map[N, Int]): NodeSet[N] with To[N] = {
            val neighs = for {
                (n, d) <- tv
                neigh  <- neighbours(n)
            } yield neigh -> (d + 1)

            val nextVisited = v ++ tv
            val found = tv.find(n => toF(n._1))

            if (found.isDefined) {
                new NodeSet[N] with To[N] {
                    override def distances = nextVisited
                    override def to = found
                }
            } else {
                val nextToVisit = neighs -- v.keys

                if (nextToVisit.isEmpty)
                    new NodeSet[N] with To[N] {
                        override def distances = nextVisited
                        override def to = None
                    }
                else search(nextVisited, nextToVisit)
            }
        }

        search(Map.empty, Map(start -> 0))
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

        pathFinder(
            start, paths, ListBuffer[N](start), collection.mutable.Set[N]()
        )
        paths.toList
    }

    def getRootNodes = graph.keys.toSet diff graph.values.flatMap(_.keys).toSet

    def removeEdgesTo(node: N) = new WeightedUndirectedGraph(
        keys.map(k =>
            k -> graph(k).-(node)
        ).toMap
    )
}
