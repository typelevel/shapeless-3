/*
 * Copyright (c) 2019 Miles Sabin
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

import org.junit.Test

trait TypeClass[A]:
  def method(a: A): Int
  def another: AnotherTypeClass[A] = method(_).toString

trait AnotherTypeClass[A]:
  def method(a: A): String

object TypeClass:
  given TypeClass[Int] = identity(_)
  given [A](using tc: TypeClass[A]): TypeClass[Single[A]] =
    s => tc.method(s.a) * 2
  given [A](using tc: TypeClass[A]): TypeClass[Pair[A]] =
    p => tc.method(p.a1) + tc.method(p.a2)

sealed trait Few[A]
case class Single[A](a: A) extends Few[A]
case class Pair[A](a1: A, a2: A) extends Few[A]

class InstancesTests:
  @Test
  def foldLeft: Unit =
    val inst = summon[K0.ProductInstances[TypeClass, Pair[Int]]]
    val p = Pair(1, 2)

    val expected = "a 1 2"
    val actual = inst.foldLeft[String](p)("a") {
      [t] => (acc: String, f: TypeClass[t], t: t) => acc + " " + f.method(t)
    }

    assert(actual == expected)

  @Test
  def foldRight: Unit =
    val inst = summon[K0.ProductInstances[TypeClass, Pair[Int]]]
    val p = Pair(1, 2)

    val expected = "a 2 1"
    val actual = inst.foldRight[String](p)("a") {
      [t] => (f: TypeClass[t], t: t, acc: String) => acc + " " + f.method(t)
    }

    assert(actual == expected)

  @Test
  def mapKforSingle: Unit =
    val inst = summon[K0.Instances[TypeClass, Single[Int]]]
    val otherInst = inst.mapK([t] => (tc: TypeClass[t]) => tc.another)
    val s = Single(42)

    val expected = "s 4242"
    val actual = otherInst.foldLeft[String](s)("s") {
      [t] => (acc: String, f: AnotherTypeClass[t], t: t) => acc + " " + f.method(t) * 2
    }

    assert(actual == expected)

  @Test
  def mapKforPair: Unit =
    val inst = summon[K0.ProductInstances[TypeClass, Pair[Int]]]
    val otherInst = inst.mapK([t] => (tc: TypeClass[t]) => tc.another)
    val p = Pair(5, 6)

    val expected = "p 66 55"
    val actual = otherInst.foldRight[String](p)("p") {
      [t] => (f: AnotherTypeClass[t], t: t, acc: String) => acc + " " + f.method(t) * 2
    }

    assert(actual == expected)

  @Test
  def mapKforFew: Unit =
    val inst = summon[K0.CoproductInstances[TypeClass, Few[Int]]]
    val otherInst = inst.mapK([t] => (tc: TypeClass[t]) => tc.another)
    val f = Pair(13, 313)

    val expected = "f 326"
    val actual = otherInst.fold[String](f) {
      [t <: Few[Int]] => (f: AnotherTypeClass[t], t: t) => "f " + f.method(t)
    }

    assert(actual == expected)
