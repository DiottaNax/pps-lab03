package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence.{Cons, Nil}

class StreamTest:

  import u03.Streams.Stream

  @Test def testTakeWhile() =
    val stream: Stream[Int] = Stream.iterate(0)(_ + 1)
    val expected = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    val actual = Stream.toList(Stream.takeWhile(stream)(_ < 5))
    assertEquals(expected, actual)

  @Test def testFill(): Unit =
    val element = "a"
    val expected = Cons(element, Cons(element, Cons(element, Nil())))
    val actual = Stream.toList(Stream.fill(3)(element))
    assertEquals(expected, actual)

  @Test def testFibonacci(): Unit =
    val fibonacci: Stream[Int] = Stream.fibonacci()
    val actual = Stream.toList(Stream.take(fibonacci)(5))
    val expected = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil())))))
    assertEquals(expected, actual)