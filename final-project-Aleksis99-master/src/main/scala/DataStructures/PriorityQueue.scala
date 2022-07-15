package DataStructures






/**
 * A class representing a Node in the Binomial queue. Not intended to be used directly.
 * @param data The actual value stored in the Node.
 * @param rank Rank of the node.
 * @param children List of children.
 * @param ord Ordering of data.
 * @tparam A Type of data.
 */
private case class Node[A](data: A, rank: Int = 0, children: List[Node[A]] = Nil)
                          (implicit val ord: Ordering[A]) extends Ordered[Node[A]] :

  /**
   * Links this node with another one and appropriately rearranges things.
   * @param other the node to link
   * @return Node with this and other linked.
   */
  def link(other: Node[A]) =
    if ord.compare(data, other.data) < 0 then Node(data, rank+1, other :: children)
    else Node(other.data, other.rank+1, this :: other.children)

  def toList : List[A] = data :: children.flatMap(_.toList)

  override def compare(that: Node[A]): Int = ord.compare(data, that.data)



//private case class  Empty[A](data: A, rank: Int = 0, children: List[Node[A]] = Nil)
//              (implicit val ord: Ordering[A]) extends Node[A]:

/**
 * This class implements a PriorityQueue using Binomial heaps.
 * @param nodes The forest of Nodes
 * @param ord Ordering of data
 * @tparam A Type of data
 */
class PriorityQueue[A] private (nodes: List[Node[A]])(implicit val ord: Ordering[A]):

  def +(x: A) : PriorityQueue[A] = PriorityQueue(insertNode(Node(x), nodes))

  def push(x: A) : PriorityQueue[A] = PriorityQueue(insertNode(Node(x), nodes))

  def pushList(xs: List[A]) : PriorityQueue[A] = xs match {
    case x :: xs => this.push(x).pushList(xs)
    case x :: Nil => this.push(x)
    case _ => this
  }
  /*def push(xs: List[A]) : PriorityQueue[A] =
    if xs.isEmpty then this
    else this.push(xs.head).push(xs.tail)*/

  def findMin: A = nodes.min.data

  def deleteMin: PriorityQueue[A] =
    val minNode = nodes.min
    PriorityQueue(meldLists(nodes.filter(_ != minNode), minNode.children.reverse))

  def pop : PriorityQueue[A] = deleteMin

  // Methods related to scala collections
  def isEmpty: Boolean = nodes.isEmpty
  def iterator: Iterator[A] = nodes.flatMap(_.toList).iterator


  private def meldLists[T](q1: List[Node[T]], q2: List[Node[T]]) : List[Node[T]] = (q1, q2) match {
    case (Nil, q) => q
    case (q, Nil) => q
    case (x :: xs, y :: ys) => if x.rank < y.rank then  x :: meldLists(xs, y :: ys)
    else if x.rank > y.rank then y :: meldLists(x :: xs, ys)
    else insertNode(x.link(y), meldLists(xs, ys))
  }

  private def insertNode[T](n: Node[T], lst: List[Node[T]]) : List[Node[T]] = lst match {
    case Nil => List(n)
    case x :: xs => if n.rank < x.rank then n :: x :: xs
    else insertNode(x.link(n), xs)
  }


//class Empty[A]

object PriorityQueue:
  def empty[A](implicit ord: Ordering[A]): PriorityQueue[A] = PriorityQueue()

  def apply[A](xs: A*)(implicit ord: Ordering[A]): PriorityQueue[A] = new PriorityQueue(xs.toList.map(x => Node(x)))

  def apply[A](nodes: List[Node[A]])(implicit ord: Ordering[A]): PriorityQueue[A] = new PriorityQueue(nodes)

  def apply[A](root: A)(implicit ord: Ordering[A]): PriorityQueue[A] = new PriorityQueue(List(Node(root)))

//  def apply[A](xs: A*)(implicit ord: Ordering[A]): PriorityQueue[A] = new PriorityQueue(xs.toList, List.empty)







