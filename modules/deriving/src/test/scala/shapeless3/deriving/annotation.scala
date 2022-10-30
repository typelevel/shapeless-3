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

import scala.annotation.Annotation as saAnnotation
import org.junit.Test
import shapeless3.test.illTyped

object AnnotationTests:

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

class AnnotationTests:
  import AnnotationTests.*

  @Test
  def simpleAnnotation(): Unit =
    val otherCC = Annotation[Other, CC].apply()
    assert(otherCC == Other())

    val lastCC = Annotation[Last, Something].apply()
    assert(lastCC == Last(true))

    val otherEnum = Annotation[Other, Control].apply()
    assert(otherEnum == Other())

    val firstEnum = Annotation[First, Control.Automatic.type].apply()
    assert(firstEnum == First())

    val secondEnum = Annotation[Second, Control.Manual].apply()
    assert(secondEnum == Second(100, "@"))

    val thirdEnum = Annotation[Third, Control.Automatic.type].apply()
    assert(thirdEnum == Third('!'))

  @Test
  def invalidAnnotation(): Unit =
    illTyped("Annotation[Other, Abstract1]", ".*no implicit argument.*")
    illTyped("Annotation[Abstract1, CC]", ".*no implicit argument.*")
    illTyped("Annotation[Abstract2, CC]", ".*no implicit argument.*")
    illTyped("Annotation[Unused, Control]", ".*no implicit argument.*")

  @Test
  def simpleAnnotations(): Unit =
    val first = Annotations[First, CC].apply()
    summon[first.type <:< (Some[First], None.type, None.type)]
    assert(first == (Some(First()), None, None))

    val second = Annotations[Second, CC].apply()
    summon[second.type <:< (None.type, None.type, Some[Second])]
    assert(second == (None, None, Some(Second(2, "b"))))

    val unused = Annotations[Unused, CC].apply()
    summon[unused.type <:< (None.type, None.type, None.type)]
    assert(unused == (None, None, None))

    val firstSum = Annotations[First, Base].apply()
    summon[firstSum.type <:< (Some[First], None.type)]
    assert(firstSum == (Some(First()), None))

    val secondSum = Annotations[Second, Base].apply()
    summon[secondSum.type <:< (None.type, Some[Second])]
    assert(secondSum == (None, Some(Second(3, "e"))))

    val firstEnum = Annotations[First, Control].apply()
    summon[firstEnum.type <:< (Some[First], None.type)]
    assert(firstEnum == (Some(First()), None))

    val secondEnum = Annotations[Second, Control].apply()
    summon[secondEnum.type <:< (None.type, Some[Second])]
    assert(secondEnum == (None, Some(Second(100, "@"))))

  @Test
  def invalidAnnotations(): Unit =
    illTyped("Annotations[Abstract1, CC]", ".*no implicit argument.*")
    illTyped("Annotations[Abstract1, Base]", ".*no implicit argument.*")
    illTyped("Annotations[Abstract2, CC]", ".*no implicit argument.*")
    illTyped("Annotations[Abstract2, Base]", ".*no implicit argument.*")
    illTyped("Annotations[Second, Abstract1]", ".*no implicit argument.*")

  @Test
  def typeAnnotations(): Unit =
    val first = TypeAnnotations[First, CC4].apply()
    summon[first.type <:< (Some[First], None.type, None.type)]
    assert(first == (Some(First()), None, None))

    val second = TypeAnnotations[Second, CC2].apply()
    summon[second.type <:< (None.type, None.type, Some[Second])]
    assert(second == (None, None, Some(Second(2, "b"))))

    val unused = TypeAnnotations[Unused, CC2].apply()
    summon[unused.type <:< (None.type, None.type, None.type)]
    assert(unused == (None, None, None))

    val firstSum = TypeAnnotations[First, Base2].apply()
    summon[firstSum.type <:< (Some[First], None.type)]
    assert(firstSum == (Some(First()), None))

    val secondSum = TypeAnnotations[Second, Base2].apply()
    summon[secondSum.type <:< (None.type, Some[Second])]
    assert(secondSum == (None, Some(Second(3, "e"))))

  @Test
  def invalidTypeAnnotations(): Unit =
    illTyped("TypeAnnotations[Dummy, CC2]", "could not find implicit value for parameter annotations: .*")
    illTyped("TypeAnnotations[Dummy, Base]", "could not find implicit value for parameter annotations: .*")
    illTyped("TypeAnnotations[Second, Dummy]", "could not find implicit value for parameter annotations: .*")

  @Test
  def allAnnotations(): Unit =
    val cc = AllAnnotations[CC3].apply() // case class
    summon[cc.type <:< (First *: EmptyTuple, EmptyTuple, (Second, Third))]
    assert(cc == (Tuple(First()), Tuple(), (Second(2, "b"), Third('c'))))

    val st = AllAnnotations[Base].apply() // sealed trait
    summon[st.type <:< (First *: EmptyTuple, Second *: EmptyTuple)]
    assert(st == (Tuple(First()), Tuple(Second(3, "e"))))

    val en = AllAnnotations[Control].apply() // enum
    summon[en.type <:< ((First, Third), Second *: EmptyTuple)]
    assert(en == ((First(), Third('!')), Tuple(Second(100, "@"))))

  @Test
  def allTypeAnnotations(): Unit =
    val st = AllTypeAnnotations[Base2].apply() // sealed trait
    summon[st.type <:< (First *: EmptyTuple, (Second, Third))]
    assert(st == (Tuple(First()), (Second(3, "e"), Third('c'))))

    val cc = AllTypeAnnotations[CC4].apply() // case class
    summon[cc.type <:< (First *: EmptyTuple, EmptyTuple, (Second, Third))]
    assert(cc == (Tuple(First()), Tuple(), (Second(2, "b"), Third('c'))))

    val user = AllTypeAnnotations[User].apply() // type refs
    summon[user.type <:< (First *: EmptyTuple, Third *: EmptyTuple)]
    assert(user == (Tuple(First()), Tuple(Third('c'))))
