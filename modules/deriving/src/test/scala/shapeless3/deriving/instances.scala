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

trait TypeClass[A] { def method(a: A): Int }
object TypeClass {
  given TypeClass[Int] = (a: Int) => a
}

case class Pair[A](a1: A, a2: A)

class InstancesTests {
  @Test
  def foldLeft: Unit = {
    val inst = summon[K0.ProductInstances[TypeClass, Pair[Int]]]
    val p = Pair(1, 2)

    val expected = "a 1 2"
    val res = inst.foldLeft [String](p)("a")([t] => (acc: String, f: TypeClass[t], t: t) => acc + " " + f.method(t))

    assert(res == expected)
  }

  @Test
  def foldRight: Unit = {
    val inst = summon[K0.ProductInstances[TypeClass, Pair[Int]]]
    val p = Pair(1, 2)

    val expected = "a 2 1"
    val res = inst.foldRight[String](p)("a")([t] => (f: TypeClass[t], t: t, acc: String) => acc + " " + f.method(t))

    assert(res == expected)
  }
}
