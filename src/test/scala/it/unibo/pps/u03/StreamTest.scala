package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence.{Cons, Nil}

class StreamTest:

  import u03.Streams.Stream
  import Stream.{cons, empty}

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

  @Test def testInterleaveWithSecondStreamBigger(): Unit =
    val s1 = cons(1, cons(3, cons(5, empty())))
    val s2 = cons(2, cons(4, cons(6, cons(8, cons(10, empty())))))
    val actual = Stream.toList(Stream.interleave(s1, s2))
    val expected = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(8, Cons(10, Nil()))))))))
    assertEquals(expected, actual)

  @Test def testInterleaveWithFirstStreamBigger(): Unit =
    val s1 = cons(1, cons(3, cons(5, cons(7, cons(9, empty())))))
    val s2 = cons(2, cons(4, cons(6, empty())))
    val actual = Stream.toList(Stream.interleave(s1, s2))
    val expected = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(9, Nil()))))))))
    assertEquals(expected, actual)
