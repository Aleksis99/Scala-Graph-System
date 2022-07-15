package GraphAlgorithms

import DataStructures.PriorityQueue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import GraphAlgorithms.*

class GraphAlgorithmsTest extends AnyFlatSpec with Matchers:

  /**
   * fullGraph
   * 0 ->
   * 1 -> 2, 5, 8
   * 2 -> 1, 3, 6
   * 3 -> 2, 4
   * 4 -> 3
   * 5 -> 6
   * 6 -> 7
   * 8 -> 9
   */
  val fullGraph: Graph = Graph(10 ,
    Map(
    1 -> List(Edge(2), Edge(5), Edge(8)),
    2 -> List(Edge(1), Edge(3), Edge(6)),
    3 -> List(Edge(2), Edge(4)),
    4 -> List(Edge(3)),
    5 -> List(Edge(6)),
    6 -> List(Edge(7)),
    8 -> List(Edge(9))).withDefaultValue(List.empty)
  )
  /**
   * fullGraph
   * 0 -> 1
   * 1 -> 2, 5, 8
   * 2 ->  3, 6
   * 3 -> 4
   * 4 -> 5
   * 5 -> 6
   * 6 -> 7
   * 8 -> 9
   */

  val bfsST = Graph.bfs(fullGraph,1)

  val path1To3 = Graph.getPathTo(1, 3, bfsST)

  "getPathTo" should "return the path " in {
    path1To3.get shouldBe List(1, 2, 3)
  }
  "getPathEdgeDistance " should "the edge distance " in {
    Graph.getPathEdgeDistance(1, 3, bfsST).get shouldBe 3
  }
  "bsf" should "be MST in unweighted graph " in {
    Graph.bfs(fullGraph, 1) shouldBe bfsST
  }

  "path from 1 to 3" should "exist" in {
    Graph.pathExists(fullGraph, 1, 3) shouldBe true
  }

  "path from 8 to 1" should "not exist" in {
    Graph.pathExists(fullGraph, 8, 1) shouldBe false
  }

  "fullGraph" should "be cyclic " in {
    Graph.cycleExists(fullGraph) shouldBe true

  }
  val dag: Graph = Graph(10 ,
    Map(
      0 -> List(Edge(1)),
      1 -> List(Edge(2), Edge(5), Edge(8)),
      2 -> List(Edge(3), Edge(6)),
      3 -> List( Edge(6)),
      4 -> List(Edge(0)),
      7 -> List(Edge(1))
    ).withDefaultValue(List.empty))
  "dag" should "be sorted topologically " in {
    Graph.topoSort(dag) shouldBe List(4, 0, 7, 1, 5, 2, 3, 6, 8, 9)
  }

  "a dag" should "not be cyclic " in {
    Graph.cycleExists(dag) shouldBe false
  }

  val fullWeightedGraph: Graph = Graph(9 ,
    Map(
      0 -> List(Edge(1, 4), Edge(7, 8)),
      1 -> List(Edge(0, 4), Edge(7, 11), Edge(2, 8)),
      2 -> List(Edge(1, 8), Edge(8, 2), Edge(3, 7)),
      3 -> List(Edge(2, 7), Edge(4, 9), Edge(5, 14)),
      4 -> List(Edge(3, 9), Edge(5, 10)),
      5 -> List(Edge(4, 10), Edge(3, 14), Edge(2, 4), Edge(6, 2)),
      6 -> List(Edge(5, 2), Edge(8, 6), Edge(7, 1)),
      7 -> List(Edge(6, 1), Edge(8, 7), Edge(1, 11), Edge(0, 8)),
      8 -> List(Edge(2, 2), Edge(6, 6), Edge(7, 7)))
  )

  "dijkstra" should "return the shortest paths and distances" in {
    Graph.dijkstra(fullWeightedGraph, 0) shouldBe (Map(
      0 -> 0,
      1 -> 0,
      2 -> 1,
      3 -> 2,
      4 -> 5,
      5 -> 6,
      6 -> 7,
      7 -> 0,
      8 -> 2
    ),
      Map(
        0 -> 0,
        1 -> 4,
        2 -> 12,
        3 -> 19,
        4 -> 21,
        5 -> 11,
        6 -> 9,
        7 -> 8,
        8 -> 14,
      ))
  }

  "MST" should "return a MST" in {
    Graph.MST(fullWeightedGraph, 0) shouldBe Map(
      0 -> 0,
      1 -> 0,
      2 -> 5,
      3 -> 2,
      4 -> 3,
      5 -> 6,
      6 -> 7,
      7 -> 0,
      8 -> 2
    )

  }

  "Bellman-Ford" should "be equal as dijkstra in a graph without negative cycles" in {
    Graph.dijkstra(fullWeightedGraph, 0) shouldBe Graph.bellmanFord(fullWeightedGraph, 0)
    Graph.dijkstra(fullWeightedGraph, 1) shouldBe Graph.bellmanFord(fullWeightedGraph, 1)
    Graph.dijkstra(fullWeightedGraph, 2) shouldBe Graph.bellmanFord(fullWeightedGraph, 2)
    Graph.dijkstra(fullWeightedGraph, 3) shouldBe Graph.bellmanFord(fullWeightedGraph, 3)
    Graph.dijkstra(fullWeightedGraph, 4) shouldBe Graph.bellmanFord(fullWeightedGraph, 4)
    Graph.dijkstra(fullWeightedGraph, 5) shouldBe Graph.bellmanFord(fullWeightedGraph, 5)
    Graph.dijkstra(fullWeightedGraph, 6) shouldBe Graph.bellmanFord(fullWeightedGraph, 6)
    Graph.dijkstra(fullWeightedGraph, 7) shouldBe Graph.bellmanFord(fullWeightedGraph, 7)
    Graph.dijkstra(fullWeightedGraph, 8) shouldBe Graph.bellmanFord(fullWeightedGraph, 8)
  }

  val fullWeightedGraphWithNegativeEdges: Graph = Graph(5 ,
    Map(
      0 -> List(Edge(1, -1), Edge(2, 4)),
      1 -> List(Edge(4, 2), Edge(3, 2), Edge(2, 3)),
      2 -> List(),
      3 -> List(Edge(2, 5), Edge(1, 1)),
      4 -> List(Edge(3, -3))
  ))

  "Bellman-Ford" should "work with graphs with negative edges" in {
    Graph.bellmanFord(fullWeightedGraphWithNegativeEdges, 0) shouldBe (Map(
      0 -> 0,
      1 -> 0,
      2 -> 1,
      3 -> 4,
      4 -> 1
    ),
      Map(
        0 -> 0,
        1 -> -1,
        2 -> 2,
        3 -> -2,
        4 -> 1
      ))
  }

  val GraphWithNegativeCycles: Graph = Graph(5 ,
    Map(
      0 -> List(Edge(1, -1), Edge(2, 4)),
      1 -> List(Edge(4, 2), Edge(3, 2), Edge(2, 3)),
      2 -> List(),
      3 -> List(Edge(2, 5), Edge(1, 1)),
      4 -> List(Edge(3, -4))
    ))

  "Bellman-Ford" should "detect negative cycles" in {
    Graph.hasNegativeCycleFromSource(fullWeightedGraphWithNegativeEdges, 0) shouldBe false
    Graph.hasNegativeCycleFromSource(GraphWithNegativeCycles, 0) shouldBe true
  }

  val weightedDag: Graph = Graph(5 ,
    Map(
      0 -> List(Edge(1, 2), Edge(2, 3)),
      1 -> List(Edge(3, -3)),
      2 -> List(Edge(1, 5), Edge(4, 1)),
      3 -> List( Edge(4, 4)),
      4 -> List()
    ))

  "shortestPathDag-Ford" should "return the shortest paths from source in a dag" in {
    Graph.shortestPathDag(weightedDag, 0) shouldBe (Map(
      0 -> 0,
      1 -> 0,
      2 -> 0,
      3 -> 1,
      4 -> 3
    ),
      Map(
        0 -> 0,
        1 -> 2,
        2 -> 3,
        3 -> -1,
        4 -> 3
      ))
  }

  "longestPathDag-Ford" should "return the longest paths from source in a dag" in {
    Graph.longestPathDag(weightedDag, 0) shouldBe (Map(
      0 -> 0,
      1 -> 2,
      2 -> 0,
      3 -> 1,
      4 -> 3
    ),
      Map(
        0 -> 0,
        1 -> 8,
        2 -> 3,
        3 -> 5,
        4 -> 9
      ))
  }