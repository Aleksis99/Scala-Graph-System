package DataStructures

import DataStructures.PriorityQueue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PriorityQueueTest extends AnyFlatSpec with Matchers:
  def ordering(x: Int, y: Int): Int =
    if x < y then -1
    else if x == y then 0
    else 1
  "an empty queue" should "produce a queue with a single element when that element is added to it" in {
    val emptyPriorityQueue = PriorityQueue.empty[Int]
    val singleElementPriorityQueue = emptyPriorityQueue + (42)

    singleElementPriorityQueue.findMin shouldBe 42
  }


  it should "throw a UnsupportedOperationException when popped" in {
    a [UnsupportedOperationException] should be thrownBy PriorityQueue.empty[Int].pop
  }

  it should "be empty" in {
    PriorityQueue.empty[Int].isEmpty shouldBe true
  }



  it should "be immutable" in {
    val emptyPriorityQueue = PriorityQueue.empty[Int]

    emptyPriorityQueue.push(42)

    emptyPriorityQueue.isEmpty shouldBe true
  }

  "a queue" should "work with a mix of operations" in {
    val queue1 = PriorityQueue.empty.push(1).push(2).push(3)
    val queue2 = queue1.pop.pop
    val queue3 = queue2.push(4).push(5).push(6).pop.push(7)

    queue1.isEmpty shouldBe false
    queue1.findMin shouldBe 1

    queue2.isEmpty shouldBe false
    queue2.findMin shouldBe 3

    queue3.isEmpty shouldBe false
    queue3.findMin shouldBe 4
  }

  it should "be able to grow, shrink to empty, and then grow again" in {
    val initialPriorityQueue = PriorityQueue.empty.push(1).push(2).push(3).push(4)

    initialPriorityQueue.isEmpty shouldBe false

    val emptiedPriorityQueue = initialPriorityQueue.pop.pop.pop.pop

    emptiedPriorityQueue.isEmpty shouldBe true
    //a [NoSuchElementException] should be thrownBy emptiedPriorityQueue.findMin

    val regrownQeueue = emptiedPriorityQueue.push(5).push(6).push(7)

    regrownQeueue.isEmpty shouldBe false
  }

  "PriorityQueue.apply(elements...)" should "create a queue from the elements" in {
    PriorityQueue[Int]().isEmpty shouldBe true
  }

