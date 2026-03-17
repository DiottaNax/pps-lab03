package it.unibo.pps.u03

import it.unibo.pps.u02.Modules.Person.{Student, Teacher}
import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Sequences.Sequence.{Cons, Nil, concat}

class Lab3Tests:

  private class SequenceTest:

    import u03.Sequences.*
    import Sequence.*

    /*******************************
     *             Task2            *
     *******************************/

    @Test def testGetCourses(): Unit =
      val student = Student("F. Diotallevi", 2026)
      val viroli = Teacher("M. Viroli", "PPS")
      val ricci = Teacher("A. Ricci", "PCD")
      val bravetti = Teacher("M. Bravetti", "LCMC")
      val courses = Cons("PPS", Cons("PCD", Cons("LCMC", Nil())))
      val sequence = Cons(viroli, Cons(ricci, Cons(student, Cons(bravetti, Nil()))))
      assertEquals(courses, getCourses(sequence))

    @Test def testFoldLeft(): Unit =
      val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
      val default = 0
      val expected = default - sum(lst)
      assertEquals(expected, foldLeft(lst)(0)(_ - _)) // -16

    @Test def testCountCourses(): Unit =
      val student = Student("F. Diotallevi", 2026)
      val viroli = Teacher("M. Viroli", "PPS")
      val ricci = Teacher("A. Ricci", "PCD")
      val bravetti = Teacher("M. Bravetti", "LCMC")
      val courses = Cons("PPS", Cons("PCD", Cons("LCMC", Nil())))
      val sequence = Cons(viroli, Cons(ricci, Cons(student, Cons(bravetti, Nil()))))
      val totalCourses = 3
      assertEquals(totalCourses, countCourses(sequence))

    @Test def testCountCoursesReturn0WithEmptyList(): Unit = assertEquals(0, countCourses(Nil()))

  private class StreamTest:

    import u03.Streams.Stream
    import Stream.{cons, empty}

    /*******************************
     *             Task3           *
     *******************************/

    @Test def testTakeWhile(): Unit =
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

    @Test def testCycle(): Unit =
      val sequence = Cons("a", Cons("b", Cons("c", Nil())))
      val repeat = Stream.cycle(sequence)
      val actual = Stream.toList(Stream.take(repeat)(15))
      val expected = concat(sequence, concat(sequence, concat(sequence, concat(sequence, sequence))))
      assertEquals(expected, actual)
