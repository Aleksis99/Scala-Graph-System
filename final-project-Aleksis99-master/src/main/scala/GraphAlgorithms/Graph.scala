package GraphAlgorithms

import DataStructures.{PriorityQueue, Queue}

import scala.annotation.tailrec

/**
 * A class representing a graph Edge.
 * @param to vertex to which the edge leads.
 * @param weight weight of the edge.
 */
case class Edge(to: Int, weight: Double = 1)


object Edge:
  def compare(a: Edge, b: Edge): Int = a.weight compare b.weight

/**
 * A class representing a graph represented using adjacency lists.
 * @param vertices vertices of the graph.
 * @param edges map mapping a vertex to the list of edges coming out of it.
 */
case class Graph(vertices: Int, edges: Map[Int,List[Edge]])


/**
 * A companion object implementing all of the main graph algorithms.
 */

object Graph:
  /**
   * Updates the predecessors of a list of edges using a predecessors list (implemented using a map).
   */

  @tailrec
  private def updatePrevious(previous: Map[Int, Int], edges: List[Int], newPrevious: Int): Map[Int, Int]=
    edges match {
      case head::tail => updatePrevious(previous + (head -> newPrevious), tail, newPrevious)
      case _ => previous
    }

  @tailrec
  private def updatePreviousNotVisited(previous: Map[Int, Int], edges: List[Int], newPrevious: Int, visited: Set[Int]): Map[Int, Int]=
    edges match {
      case head::tail =>
        if !visited(head) then
        updatePreviousNotVisited(previous + (head -> newPrevious), tail, newPrevious, visited)
        else updatePreviousNotVisited(previous, tail, newPrevious, visited)

      //case head::Nil => previous + (head -> newPrevious)
      case _ => previous
    }

  /**
   * Initializes the predecessors of the graph edges using a predecessors list (implemented using a map).
   * Making each edge its own predecessor.
   */
  private def initializePrevious(g: Graph): Map[Int, Int] =
    @tailrec
    def initializePreviousHelper(paths: Map[Int, Int], vertices: Int): Map[Int, Int] =
      if vertices == 0 then paths + (vertices -> vertices)
      else initializePreviousHelper(paths + (vertices -> vertices ), vertices - 1)

    initializePreviousHelper(Map.empty[Int,Int], g.vertices - 1)

  /**
   * Initializes the distances of the graph edges to "infinity" using a distance list (implemented using a map).
   */
  private def initializeDistance(g: Graph, initial: Double = Double.MaxValue): Map[Int, Double] =
    @tailrec
    def initializeDistanceHelper(distance: Map[Int, Double], vertices: Int): Map[Int, Double] =
      if vertices == 0 then distance + (vertices -> initial)
      else initializeDistanceHelper(distance + (vertices -> initial ), vertices - 1)

    initializeDistanceHelper(Map.empty[Int,Double], g.vertices - 1)


  def getPathTo(from: Int, to: Int, previous: Map[Int, Int]): Option[List[Int]] =
    @tailrec
    def getPathToHelp(from: Int, curr: Int, path: List[Int]): List[Int] =
      if curr == from then path
      else if previous(curr) == curr then path
      else getPathToHelp(from, previous(curr), previous(curr)::path)

    val path = getPathToHelp(from, to, List(to))
    if path.head == from then Some(path)
    else None

  def getPathEdgeDistance(from: Int, to: Int, previous: Map[Int, Int]): Option[Double] =
    @tailrec
    def getPathEdgeDistanceHelp(from: Int, curr: Int, path: List[Int], distance: Double): (List[Int], Double) =
      if curr == from then (path, distance + 1)
      else if previous(curr) == curr then (path, distance)
      else getPathEdgeDistanceHelp(from, previous(curr), previous(curr)::path, distance + 1)

    val pathAndWeight = getPathEdgeDistanceHelp(from, to, List(to), 0)
    if pathAndWeight._1.head == from then Some(pathAndWeight._2)
    else None  


  def pathExists(g: Graph, from: Int, to: Int): Boolean =
    def dfs(curr: Int, visited: Set[Int]): Set[Int] =
      if visited(curr) then visited
      else
        g.edges(curr).foldLeft(visited + curr)((visited, edge) => dfs(edge.to, visited))
    dfs(from, Set.empty[Int])(to)

  def cycleExists(g: Graph): Boolean =
    def dfs(curr: Int, visited: Set[Int], stack: Set[Int], hasCycle: Boolean): (Set[Int], Set[Int], Boolean) =
      //if visited(curr) && stack(curr) then (visited, stack, true)
      if visited(curr) then (visited, stack - curr, hasCycle)
      else if g.edges(curr).exists(x => stack(x.to)) then (visited, stack, true)
      else
        g.edges(curr).filter(x => !visited(x.to)).foldLeft((visited + curr, stack + curr, hasCycle))((visitedStackHasCycle: (Set[Int], Set[Int], Boolean), edge: Edge)
        => dfs(edge.to, visitedStackHasCycle._1, visitedStackHasCycle._2, visitedStackHasCycle._3))
    @tailrec
    def cycleExistsHelp(curr: Int, visited: Set[Int]): Boolean =
      if !visited(curr) then
        val fromCurr = dfs(curr, visited: Set[Int],  Set.empty[Int], false)
        if fromCurr._3 then true
        else if curr < g.vertices then cycleExistsHelp(curr + 1, fromCurr._1)
        else false
      else if  curr < g.vertices then cycleExistsHelp(curr + 1, visited)
      else false

    cycleExistsHelp(0, Set.empty[Int])

  /**
   * Returns returns a list of the vertices of the graph in topological order.
   */
  def topoSort(graph: Graph): List[Int] =
    def dfs(curr: Int, visited: Set[Int], stack: List[Int]): (Set[Int], List[Int]) =
      if visited(curr) then (visited, stack)
      else
        val (newVisited, newStack) = graph.edges(curr).foldLeft((visited + curr, stack))((visitedStack: (Set[Int], List[Int]), edge: Edge) => dfs(edge.to, visitedStack._1, visitedStack._2))
        (newVisited, curr::newStack)

    def topoSortHelp(curr: Int, visited: Set[Int], stack: List[Int]): (Set[Int], List[Int]) =
      if curr < 0 then (visited, stack)
      else if visited(curr) then topoSortHelp(curr - 1, visited, stack)
      else
        val (newVisited, newStack) = dfs(curr, visited, stack)
        topoSortHelp(curr - 1, newVisited, newStack)

    topoSortHelp(graph.vertices - 1, Set.empty[Int], List.empty[Int])._2

  /**
   * Returns a spanning tree from the given source for graphs.
   * That also represents all shortest paths from the given source for unweighted graphs.
   */
  def bfs(g: Graph, start: Int): Map[Int, Int] =
    @tailrec
    def bfsHelp(toVisit: Queue[Int], visited: Set[Int], previous: Map[Int, Int]): Map[Int, Int] =
      if toVisit.isEmpty then previous
      else
        val current = toVisit.peek

        if visited(current) then bfsHelp(toVisit.pop, visited, previous)
        else
          val neighbours = g.edges(current).map(g => g.to)

          bfsHelp(
          toVisit.pop.push(neighbours),
          visited + current,
            updatePreviousNotVisited(previous,neighbours,current, visited)
          /*previous map {
            case (key, value) =>
            if neighbours.contains(key) then (key, current)
            else(key, value)}*/
        )

    bfsHelp(Queue(start), Set.empty[Int], initializePrevious(g))
  /**
   * @param g the graph.
   * @param previous the predecessors of each vertex.
   * @param distances the distances to each edge from the source.
   * @param edges edges to relax.
   * @param curr the current vertex.
   * Relaxes all the edges from a given vertex.
   * (This is the Relax function know from Introduction to Algorithms CLRS)
   */
  @tailrec
  private def relax(g: Graph, previous: Map[Int, Int], distances: Map[Int, Double], edges: List[Edge], curr: Int):
    (Map[Int, Int], Map[Int, Double]) =
    edges match {
      case head::tail => {
        val newDist = distances(curr) + head.weight
        if newDist < distances(head.to) && distances(curr) != Double.MaxValue then
          relax(g: Graph, previous + (head.to -> curr), distances + (head.to -> newDist), tail, curr)
        else  relax(g: Graph, previous, distances , tail, curr)
      }
      /*case head::Nil => {
        val newDist = distances(curr) + head.weight
        if newDist < distances(head.to) then
          (previous + (head.to -> curr), distances + (head.to -> newDist))
        else  (previous , distances )
      }*/
      case _ => (previous , distances )
    }
  @tailrec
  private def relaxReversed(g: Graph, previous: Map[Int, Int], distances: Map[Int, Double], edges: List[Edge], curr: Int):
  (Map[Int, Int], Map[Int, Double]) =
    edges match {
      case head::tail => {
        val newDist = distances(curr) + head.weight
        if newDist > distances(head.to) && distances(curr) != Double.MinValue then
          relaxReversed(g: Graph, previous + (head.to -> curr), distances + (head.to -> newDist), tail, curr)
        else  relax(g: Graph, previous, distances , tail, curr)
      }
      /*case head::Nil => {
        val newDist = distances(curr) + head.weight
        if newDist < distances(head.to) then
          (previous + (head.to -> curr), distances + (head.to -> newDist))
        else  (previous , distances )
      }*/
      case _ => (previous , distances )
    }
  @tailrec
  private def relaxDijkstra(g: Graph, toVisit: PriorityQueue[Edge], previous: Map[Int, Int], distances: Map[Int, Double], edges: List[Edge], curr: Int):
  (PriorityQueue[Edge], Map[Int, Int], Map[Int, Double]) =
    edges match {
      case head::tail => {
        val newDist = distances(curr) + head.weight
        if newDist < distances(head.to) && distances(curr) != Double.MaxValue then
          relaxDijkstra(g: Graph, toVisit.push(Edge(head.to, newDist)), previous + (head.to -> curr), distances + (head.to -> newDist), tail, curr)
        else  relaxDijkstra(g: Graph, toVisit, previous, distances , tail, curr)
      }
      /*case head::Nil => {
        val newDist = distances(curr) + head.weight
        if newDist < distances(head.to) then
          (previous + (head.to -> curr), distances + (head.to -> newDist))
        else  (previous , distances )
      }*/
      case _ => (toVisit, previous , distances )
    }

  @tailrec
  private def canRelax(g: Graph, previous: Map[Int, Int], distances: Map[Int, Double], edges: List[Edge], curr: Int):
  Boolean =
    edges match {
      case head::tail => {
        val newDist = distances(curr) + head.weight
        if newDist < distances(head.to) then true
        else  canRelax(g: Graph, previous, distances , tail, curr)
      }/*
      case head::Nil => {
        val newDist = distances(curr) + head.weight
        if newDist < distances(head.to) then true
        else  false
      }*/
      case _ => false
    }
  /**
   * Finds all the shortest paths from a given source.
   * Returns a predecessors list  and distances list (realized as maps).
   */
  def dijkstra(g: Graph, source: Int): (Map[Int, Int], Map[Int, Double]) =
    @tailrec
    def dijkstraHelp(toVisit: PriorityQueue[Edge], visited: Set[Int], previous: Map[Int, Int], distances: Map[Int, Double]):
    (Map[Int, Int], Map[Int, Double]) =
      if toVisit.isEmpty then (previous, distances)
      else
        val current = toVisit.findMin

        if visited(current.to) then dijkstraHelp(toVisit.pop, visited, previous, distances)
        else
          val neighbours = g.edges(current.to)//.filter(e => !visited(e.to))
          val (newToVisit, newPrevious, newDistances) = relaxDijkstra(g, toVisit.pop, previous, distances, neighbours, current.to)
          dijkstraHelp(
            newToVisit,
            visited + current.to,
            newPrevious,
            newDistances
          )

    dijkstraHelp(PriorityQueue(Edge(source,0))(Edge.compare), Set.empty[Int], initializePrevious(g), initializeDistance(g) + (source -> 0))
  /**
   * Finds all the shortest paths from a given source in a DAG(Directed acyclic graph).
   */
  def shortestPathDag(graph: Graph, source: Int): (Map[Int, Int], Map[Int, Double]) =
    def shortestPathDagHelp(curr: List[Int], previous: Map[Int, Int], distances: Map[Int, Double]):
    (Map[Int, Int], Map[Int, Double]) =
      if curr.isEmpty then (previous, distances)
      else
        val neighbours = graph.edges(curr.head)
        val (newPrevious, newDistances) = relax(graph, previous, distances, neighbours, curr.head)
        shortestPathDagHelp(curr.tail, newPrevious, newDistances)

    val topoSorted = topoSort(graph)
    shortestPathDagHelp(topoSorted, initializePrevious(graph), initializeDistance(graph) + (source -> 0))


  /**
   * Finds all the longest paths from a given source in a DAG(Directed acyclic graph).
   */
  def longestPathDag(graph: Graph, source: Int): (Map[Int, Int], Map[Int, Double]) =
    def longestPathDagHelp(curr: List[Int], previous: Map[Int, Int], distances: Map[Int, Double]):
    (Map[Int, Int], Map[Int, Double]) =
      if curr.isEmpty then (previous, distances)
      else
        val neighbours = graph.edges(curr.head)
        val (newPrevious, newDistances) = relaxReversed(graph, previous, distances, neighbours, curr.head)
        longestPathDagHelp(curr.tail, newPrevious, newDistances)

    val topoSorted = topoSort(graph)
    longestPathDagHelp(topoSorted, initializePrevious(graph), initializeDistance(graph, Double.MinValue) + (source -> 0))
  /**
   * Finds the MST(Minimum spanning tree) from a given source using Prims algorithm.
   */
  def MST(g: Graph, source: Int): Map[Int, Int] =
    @tailrec
    def updatePrevious(g: Graph, previous: Map[Int, Int], distances: Map[Int, Double], edges: List[Edge], visited: Set[Int], curr: Int):
      (Map[Int, Int], Map[Int, Double]) =
      edges match {
        case head::tail => {
          val newDist = head.weight
          if newDist < distances(head.to) && !visited(head.to) then
            updatePrevious(g: Graph, previous + (head.to -> curr), distances + (head.to -> newDist), tail, visited, curr)
          else  updatePrevious(g: Graph, previous, distances , tail, visited, curr)
        }
        case head::Nil => {
          val newDist = head.weight
          if newDist < distances(head.to) then
            (previous + (head.to -> curr), distances + (head.to -> newDist))
          else  (previous , distances )
        }
        case _ => (previous , distances )
      }
    @tailrec
    def primMSTHelp(toVisit: PriorityQueue[Edge], visited: Set[Int], previous: Map[Int, Int], distances: Map[Int, Double]):
     Map[Int, Int]=
      if toVisit.isEmpty then previous
      else
        val current = toVisit.findMin

        if visited(current.to) then primMSTHelp(toVisit.pop, visited, previous, distances)
        else
          val neighbours = g.edges(current.to)
          val (newPrevious, newDistances) = updatePrevious(g, previous, distances, neighbours, visited + current.to, current.to)
          primMSTHelp(
            toVisit.pop.pushList(neighbours),
            visited + current.to,
            newPrevious,
            newDistances
          )
    primMSTHelp(PriorityQueue(Edge(source,0))(Edge.compare), Set.empty[Int], initializePrevious(g), initializeDistance(g))
  /**
   * Finds single source all shortest path like dijkstra but also works with negative weights.
   */
  def bellmanFord(g: Graph, source: Int): (Map[Int, Int], Map[Int, Double]) =

    @tailrec
    def bellmanFordIterate(previous: Map[Int, Int], distances: Map[Int, Double], current: Int):
    (Map[Int, Int], Map[Int, Double]) =
      if current < 0 then (previous, distances)
      else
        val neighbours = g.edges(current)
        val (newPrevious, newDistances) = relax(g, previous, distances, neighbours, current)
        bellmanFordIterate(
          newPrevious,
          newDistances,
          current - 1
        )
    @tailrec
    def bellmanFordHelp(previous: Map[Int, Int], distances: Map[Int, Double], repeat: Int):
    (Map[Int, Int], Map[Int, Double])=
      val neighbours = g.edges.flatMap( (k, v) => v)
      if repeat > 0 then
        val (newPrevious, newDistances) = bellmanFordIterate(previous, distances, g.vertices - 1)
        bellmanFordHelp(newPrevious, newDistances, repeat - 1)
      else
        (previous, distances)

    bellmanFordHelp(initializePrevious(g), initializeDistance(g) + (source -> 0), g.vertices - 1)
  /**
   * Finds is there is a negative cycle from the given source using Bellman-Fords algorithm.
   */
  def hasNegativeCycleFromSource(g: Graph, source: Int): Boolean =
    @tailrec
    def hasNegativeCycleHelp(previous: Map[Int, Int], distances: Map[Int, Double], current: Int):
    Boolean =
      if current < 0 then false
      else
        val neighbours = g.edges(current)
        if canRelax(g, previous, distances, neighbours, current) then true
        else
          hasNegativeCycleHelp(
            previous,
            distances,
            current - 1
          )
    val (previous, distances) = bellmanFord(g, source)
    hasNegativeCycleHelp(previous, distances, g.vertices - 1)

  /*def floydWarshall(g: Graph): Map[Int, Map[Int,Double]] =
    def initializeAdjMatrix(g: Graph): Map[Int, Map[Int,Double]]= */













