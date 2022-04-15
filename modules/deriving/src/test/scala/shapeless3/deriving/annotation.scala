/*
 * Copyright (c) 2015-19 Alexandre Archambault
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless3.deriving

import scala.annotation.{ Annotation => saAnnotation }
import org.junit.Test
import shapeless3.test.illTyped

object AnnotationTestsDefinitions {

  case class First() extends saAnnotation
  case class Second(i: Int, s: String) extends saAnnotation
  case class Third(c: Char) extends saAnnotation

  case class Other() extends saAnnotation
  case class Last(b: Boolean) extends saAnnotation

  case class Unused() extends saAnnotation

  @Other case class CC(
    @First i: Int,
    s: String,
    @Second(2, "b") ob: Option[Boolean]
  )

  @Last(true) trait Something

  sealed trait Base
  @First case class BaseI(i: Int) extends Base
  @Second(3, "e") case class BaseS(s: String) extends Base

  trait Abstract1
  abstract class Abstract2

  sealed trait Base2
  case class BaseI2(i: Int) extends Base2 @First
  case class BaseS2(s: String) extends Base2 @Second(3, "e") @Third('c')

  trait Dummy

  case class CC2(
    i: Int @First,
    s: String,
    ob: Option[Boolean] @Second(2, "b")
  )

  case class CC3(
    @First i: Int,
    s: String,
    @Second(2, "b") @Third('c') ob: Option[Boolean]
  )

  case class CC4(
    i: Int @First,
    s: String,
    ob: Option[Boolean] @Second(2, "b") @Third('c')
  )

  type PosInt = Int @First
  type Email = String @Third('c')
  case class User(age: PosInt, email: Email)

  @Other @Last(false) enum Control:
    @First @Third('!') case Automatic
    @Second(100, "@") case Manual(age: PosInt, email: Email)
}

class AnnotationTests {
  import AnnotationTestsDefinitions._

  @Test
  def simpleAnnotation: Unit = {
    {
      val other = Annotation[Other, CC].apply()
      assert(other == Other())

      val last = Annotation[Last, Something].apply()
      assert(last == Last(true))
    }

    {
      val other: Other = Annotation[Other, CC].apply()
      assert(other == Other())

      val last: Last = Annotation[Last, Something].apply()
      assert(last == Last(true))
    }

    {
      val other = Annotation[Other, Control].apply()
      assert(other == Other())

      val first = Annotation[First, Control.Automatic.type].apply()
      assert(first == First())

      val second = Annotation[Second, Control.Manual].apply()
      assert(second == Second(100, "@"))

      val third = Annotation[Third, Control.Automatic.type].apply()
      assert(third == Third('!'))
    }
  }

  @Test
  def invalidAnnotation: Unit = {
    illTyped("Annotation[Other, Abstract1]", ".*no implicit argument.*")
    illTyped("Annotation[Abstract1, CC]", ".*no implicit argument.*")
    illTyped("Annotation[Abstract2, CC]", ".*no implicit argument.*")
    illTyped("Annotation[Unused, Control]", ".*no implicit argument.*")
  }

  @Test
  def simpleAnnotations: Unit = {
    {
      val first: (Some[First], None.type, None.type) = Annotations[First, CC].apply()
      assert(first == (Some(First()), None, None))

      val second: (None.type, None.type, Some[Second]) = Annotations[Second, CC].apply()
      assert(second == (None, None, Some(Second(2, "b"))))

      val unused: (None.type, None.type, None.type) = Annotations[Unused, CC].apply()
      assert(unused == (None, None, None))

      val firstSum: (Some[First], None.type) = Annotations[First, Base].apply()
      assert(firstSum == (Some(First()), None))

      val secondSum: (None.type, Some[Second]) = Annotations[Second, Base].apply()
      assert(secondSum == (None, Some(Second(3, "e"))))
    }

    {
      val first = Annotations[First, CC].apply()
      assert(first == (Some(First()), None, None))

      val second = Annotations[Second, CC].apply()
      assert(second == (None, None, Some(Second(2, "b"))))

      val unused = Annotations[Unused, CC].apply()
      assert(unused == (None, None, None))

      val firstSum = Annotations[First, Base].apply()
      assert(firstSum == (Some(First()), None))

      val secondSum = Annotations[Second, Base].apply()
      assert(secondSum == (None, Some(Second(3, "e"))))
    }
  }

  @Test
  def invalidAnnotations: Unit = {
    illTyped("Annotations[Abstract1, CC]", ".*no implicit argument.*")
    illTyped("Annotations[Abstract1, Base]", ".*no implicit argument.*")
    illTyped("Annotations[Abstract2, CC]", ".*no implicit argument.*")
    illTyped("Annotations[Abstract2, Base]", ".*no implicit argument.*")
    illTyped("Annotations[Second, Abstract1]", ".*no implicit argument.*")
  }

  @Test
  def typeAnnotations: Unit = {
    {
      val first: (Some[First], None.type, None.type) = TypeAnnotations[First, CC4].apply()
      assert(first == (Some(First()), None, None))

      val second: (None.type, None.type, Some[Second]) = TypeAnnotations[Second, CC2].apply()
      assert(second == (None, None, Some(Second(2, "b"))))

      val unused: (None.type, None.type, None.type) = TypeAnnotations[Unused, CC2].apply()
      assert(unused == (None, None, None))

      val firstSum: (Some[First], None.type) = TypeAnnotations[First, Base2].apply()
      assert(firstSum == (Some(First()), None))

      val secondSum: (None.type, Some[Second]) = TypeAnnotations[Second, Base2].apply()
      assert(secondSum == (None, Some(Second(3, "e"))))
    }

    {
      val first = TypeAnnotations[First, CC2].apply()
      assert(first == (Some(First()), None, None))

      val second = TypeAnnotations[Second, CC2].apply()
      assert(second == (None, None, Some(Second(2, "b"))))

      val unused = TypeAnnotations[Unused, CC2].apply()
      assert(unused == (None, None, None))

      val firstSum = TypeAnnotations[First, Base2].apply()
      assert(firstSum == (Some(First()), None))

      val secondSum = TypeAnnotations[Second, Base2].apply()
      assert(secondSum == (None, Some(Second(3, "e"))))
    }
  }

  @Test
  def invalidTypeAnnotations: Unit = {
    illTyped("TypeAnnotations[Dummy, CC2]", "could not find implicit value for parameter annotations: .*")
    illTyped("TypeAnnotations[Dummy, Base]", "could not find implicit value for parameter annotations: .*")
    illTyped("TypeAnnotations[Second, Dummy]", "could not find implicit value for parameter annotations: .*")
  }

  @Test
  def allAnnotations: Unit = {
    type T1First = First *: EmptyTuple
    val first: T1First = First() *: EmptyTuple

    val cc: (T1First, EmptyTuple, (Second, Third)) = AllAnnotations[CC3].apply()
    assert(cc == (first, EmptyTuple, (Second(2, "b"), Third('c'))))

    type T1Second = Second *: EmptyTuple
    val second: T1Second = Second(3, "e") *: EmptyTuple

    val st: (T1First, T1Second) = AllAnnotations[Base].apply()
    assert(st == (first, second))
  }

  @Test
  def allTypeAnnotations: Unit = {
    type T1First = First *: EmptyTuple
    val first: T1First = First() *: EmptyTuple

    val st: (T1First, (Second, Third)) = AllTypeAnnotations[Base2].apply() // sealed trait
    assert(st == (first, (Second(3, "e"), Third('c'))))

    val cc: (T1First, EmptyTuple, (Second, Third)) = AllTypeAnnotations[CC4].apply() // case class
    assert(cc == (first, EmptyTuple, (Second(2, "b"), Third('c'))))

    type T1Third = Third *: EmptyTuple
    val third: T1Third = Third('c') *: EmptyTuple

    val user: (T1First, T1Third) = AllTypeAnnotations[User].apply() // type refs
    assert(user == (first, third))
  }
}
