package DataStructures



class Queue[A] private (front: List[A], back: List[A]) extends Iterable[A]:
  def peek: A =
    if front.nonEmpty then front.head
    else back.last

  def push(a: A): Queue[A] = new Queue(front, a :: back)
  def push(as: Seq[A]) = new Queue(front, as.toList.reverse ::: back)

  def pop: Queue[A] =
    if front.nonEmpty then new Queue(front.tail, back)
    else new Queue(back.reverse.tail, List.empty)

  override def isEmpty: Boolean = front.isEmpty && back.isEmpty
  override def size: Int = front.size + back.size

  def iterator: Iterator[A] = front.iterator ++ back.reverseIterator

object Queue:
  def empty[A]: Queue[A] = Queue()

  def apply[A](xs: A*): Queue[A] = new Queue(xs.toList, List.empty)
  
