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

package shapeless3.deriving.internals

import scala.annotation.tailrec
import scala.compiletime.*
import scala.deriving.*

import shapeless3.deriving.*

private[shapeless3] abstract class ErasedInstances[K, FT] extends Serializable {
  type Pure[F[_]] = [a] => a => F[a]
  type Map[F[_]] = [a, b] => (F[a], a => b) => F[b]
  type Ap[F[_]] = [a, b] => (F[a => b], F[a]) => F[b]
  type TailRecM[F[_]] = [a, b] => (a, a => F[Either[a, b]]) => F[b]

  def erasedMapK(f: Any => Any): ErasedInstances[K, ?]
  def erasedMap(x: Any)(f: (Any, Any) => Any): Any
  def erasedTraverse[F[_]](x: Any)(map: Map[F])(pure: Pure[F])(ap: Ap[F])(f: (Any, Any) => F[Any]): F[Any]
}

private[shapeless3] abstract class ErasedProductInstances[K, FT] extends ErasedInstances[K, FT] {
  def erasedMapK(f: Any => Any): ErasedProductInstances[K, _]
  def erasedConstruct(f: Any => Any): Any
  def erasedConstructA[F[_]](f: Any => F[Any])(pure: Pure[F], map: Map[F], ap: Ap[F]): F[Any]
  def erasedConstructM[F[_]](f: Any => F[Any])(pure: Pure[F], map: Map[F], tailRecM: TailRecM[F]): F[Any]
  def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any])
  def erasedMap(x0: Any)(f: (Any, Any) => Any): Any
  def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any
  def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any
  def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any
  def erasedFoldRight(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any
  def erasedFoldRight2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any
  def erasedProject(x0: Any)(p: Int)(f: (Any, Any) => Any): Any
}

private[shapeless3] final class ErasedProductInstances1[K, FT](val mirror: Mirror.Product, i0: () => Any) extends ErasedProductInstances[K, FT] {

  @deprecated("Preserved for bincompat reasons. DO NOT USE as it will lead to stack overflows when deriving instances for recursive types")
  def this(mirror: Mirror.Product, i0: Any) = this(mirror, () => i0)

  lazy val i = i0()

  inline def toProduct(x: Any): Product = x.asInstanceOf[Product]
  private def fromElement(x: Any) = mirror.fromProduct(Tuple1(x))
  
  final def erasedMapK(f: Any => Any): ErasedProductInstances[K, ?] =
    new ErasedProductInstances1(mirror, () => f(i))

  final def erasedConstruct(f: Any => Any): Any =
    fromElement(f(i))

  final def erasedConstructA[F[_]](f: Any => F[Any])(pure: Pure[F], map: Map[F], ap: Ap[F]): F[Any] =
    map(f(i), fromElement)

  final def erasedConstructM[F[_]](f: Any => F[Any])(pure: Pure[F], map: Map[F], tailRecM: TailRecM[F]): F[Any] =
    map(f(i), fromElement)

  final def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
    val (acc0, e0) = f(a, i)
    e0 match {
      case Some(_) => (acc0, Some(mirror.fromProduct(e0)))
      case None => (acc0, None)
    }
  }

  final def erasedMap(x0: Any)(f: (Any, Any) => Any): Any =
    fromElement(f(i, toProduct(x0).productElement(0)))

  final def erasedTraverse[F[_]](x: Any)(map: Map[F])(pure: Pure[F])(ap: Ap[F])(f: (Any, Any) => F[Any]): F[Any] =
    map(f(i, toProduct(x).productElement(0)), fromElement)

  final def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any =
    fromElement(f(i, toProduct(x0).productElement(0), toProduct(y0).productElement(0)))

  final def erasedFoldLeft(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
    f(a, i, toProduct(x0).productElement(0)) match {
      case Complete(r) => r
      case acc => acc
    }
  }

  final def erasedFoldLeft2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
    f(a, i, toProduct(x0).productElement(0), toProduct(y0).productElement(0)) match {
      case Complete(r) => r
      case acc => acc
    }
  }

  final def erasedFoldRight(x0: Any)(a: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
    f(i, toProduct(x0).productElement(0), a) match {
      case Complete(r) => r
      case acc => acc
    }
  }

  final def erasedFoldRight2(x0: Any, y0: Any)(a: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
    f(i, toProduct(x0).productElement(0), toProduct(y0).productElement(0), a) match {
      case Complete(r) => r
      case acc => acc
    }
  }

  final def erasedProject(x0: Any)(p: Int)(f: (Any, Any) => Any): Any =
    f(i, toProduct(x0).productElement(0))
}

object ErasedProductInstances1 {
  def apply[K, FT](mirror: Mirror.Product, i: => Any): ErasedProductInstances1[K, FT] =
    new ErasedProductInstances1(mirror, () => i)
}

private[shapeless3] final class ErasedProductInstancesN[K, FT](val mirror: Mirror.Product, is0: () => Array[Any]) extends ErasedProductInstances[K, FT] {
  import ErasedProductInstances._

  @deprecated("Preserved for bincompat reasons. DO NOT USE as it will lead to stack overflows when deriving instances for recursive types")
  def this(mirror: Mirror.Product, is0: Array[Any]) = this(mirror, () => is0)

  lazy val is: Array[Any] = is0()

  inline def toProduct(x: Any): Product = x.asInstanceOf[Product]
  private def fromArray(xs: Array[Any]) = mirror.fromProduct(new ArrayProduct(xs))
  private def fromIndexedSeq(xs: IndexedSeq[Any]) = mirror.fromProduct(new IndexedSeqProduct(xs))
  private def fromEmptyProduct = mirror.fromProduct(None)

  private def traverseProduct[F[_]](x: Product, f: (Any, Any) => F[Any])(pure: Pure[F], map: Map[F], ap: Ap[F]): F[Any] =
    val n = is.length
    if n == 0 then pure(x)
    else
      var acc = pure(Vector.empty[Any])
      var i = 0
      while i < n do
        acc = ap(map(acc, _.appended), f(is(i), x.productElement(i)))
        i += 1
      map(acc, fromIndexedSeq)
  end traverseProduct

  final def erasedMapK(f: Any => Any): ErasedProductInstances[K, ?] =
    new ErasedProductInstancesN(mirror, () => is.map(f))

  final def erasedConstruct(f: Any => Any): Any = {
    val n = is.length
    if (n == 0) fromEmptyProduct
    else {
      val arr = new Array[Any](n)
      var i = 0
      while(i < n) {
        arr(i) = f(is(i))
        i = i+1
      }
      fromArray(arr)
    }
  }

  final def erasedConstructA[F[_]](f: Any => F[Any])(pure: Pure[F], map: Map[F], ap: Ap[F]): F[Any] =
    traverseProduct(new DummyProduct(is.length), (tc, _) => f(tc))(pure, map, ap)

  final def erasedConstructM[F[_]](f: Any => F[Any])(pure: Pure[F], map: Map[F], tailRecM: TailRecM[F]): F[Any] =
    val n = is.length
    def step(xs: Vector[Any]) =
      val i = xs.length
      if i >= n then pure(Right(xs))
      else map(f(is(i)), a => Left(xs :+ a))

    if n == 0 then pure(fromEmptyProduct)
    else map(tailRecM(Vector.empty[Any], step), fromIndexedSeq)
  end erasedConstructM

  final def erasedUnfold(a: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) = {
    val n = is.length
    if (n == 0) (a, Some(fromEmptyProduct))
    else {
      val arr = new Array[Any](n)
      var acc = a
      var i = 0
      while(i < n) {
        val (acc0, e0) = f(acc, is(i))
        e0 match {
          case Some(e) =>
            acc = acc0
            arr(i) = e
          case None =>
            return (acc0, None)
        }
        i = i+1
      }
      (acc, Some(fromArray(arr)))
    }
  }

  final def erasedMap(x0: Any)(f: (Any, Any) => Any): Any = {
    val n = is.length
    if (n == 0) x0
    else {
      val x = toProduct(x0)
      val arr = new Array[Any](n)
      var i = 0
      while(i < n) {
        arr(i) = f(is(i), x.productElement(i))
        i = i+1
      }
      fromArray(arr)
    }
  }

  final def erasedTraverse[F[_]](x: Any)(map: Map[F])(pure: Pure[F])(ap: Ap[F])(f: (Any, Any) => F[Any]): F[Any] =
    traverseProduct(toProduct(x), f)(pure, map, ap)

  final def erasedMap2(x0: Any, y0: Any)(f: (Any, Any, Any) => Any): Any = {
    val n = is.length
    if (n == 0) x0
    else {
      val x = toProduct(x0)
      val y = toProduct(y0)
      val arr = new Array[Any](n)
      var i = 0
      while(i < n) {
        arr(i) = f(is(i), x.productElement(i), y.productElement(i))
        i = i+1
      }
      fromArray(arr)
    }
  }

  final def erasedFoldLeft(x0: Any)(i: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
    val n = is.length
    if (n == 0) i
    else {
      val x = toProduct(x0)
      @tailrec
      def loop(i: Int, acc: Any): Any =
        if(i >= n) acc
        else
          f(acc, is(i), x.productElement(i)) match {
            case Complete(r) => r
            case acc =>
              loop(i+1, acc)
          }

      loop(0, i)
    }
  }

  final def erasedFoldRight(x0: Any)(i: Any)(f: (Any, Any, Any) => CompleteOr[Any]): Any = {
    val n = is.length
    if (n == 0) i
    else {
      val x = toProduct(x0)
      @tailrec
      def loop(i: Int, acc: Any): Any =
        if(i < 0) acc
        else
          f(is(i), x.productElement(i), acc) match {
            case Complete(r) => r
            case acc =>
              loop(i-1, acc)
          }

      loop(n-1, i)
    }
  }

  final def erasedFoldLeft2(x0: Any, y0: Any)(i: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
    val n = is.length
    if (n == 0) i
    else {
      val x = toProduct(x0)
      val y = toProduct(y0)
      @tailrec
      def loop(i: Int, acc: Any): Any =
        if(i >= n) acc
        else
          f(acc, is(i), x.productElement(i), y.productElement(i)) match {
            case Complete(r) => r
            case acc =>
              loop(i+1, acc)
          }

      loop(0, i)
    }
  }

  final def erasedFoldRight2(x0: Any, y0: Any)(i: Any)(f: (Any, Any, Any, Any) => CompleteOr[Any]): Any = {
    val n = is.length
    if (n == 0) i
    else {
      val x = toProduct(x0)
      val y = toProduct(y0)
      @tailrec
      def loop(i: Int, acc: Any): Any =
        if(i < 0) acc
        else
          f(is(i), x.productElement(i), y.productElement(i), acc) match {
            case Complete(r) => r
            case acc =>
              loop(i-1, acc)
          }

      loop(n-1, i)
    }
  }

  final def erasedProject(x0: Any)(p: Int)(f: (Any, Any) => Any): Any =
    f(is(p), toProduct(x0).productElement(p))
}

object ErasedProductInstancesN {
  def apply[K, FT](mirror: Mirror.Product, is: => Array[Any]): ErasedProductInstancesN[K, FT] =
    new ErasedProductInstancesN(mirror, () => is)
}

private[shapeless3] object ErasedProductInstances {
  class ArrayProduct(val elems: Array[Any]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int): Any = elems(n)
    def productArity: Int = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
  }

  final class IndexedSeqProduct(val elems: IndexedSeq[Any]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int): Any = elems(n)
    def productArity: Int = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
  }

  final class DummyProduct(n: Int) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int): Any = null
    def productArity: Int = n
  }

  inline def summonOne[T] = inline erasedValue[T] match {
    case _: Tuple1[a] => summonInline[a]
  }

  val emptyArray: Array[Any] = new Array(0)

  inline def apply[K, FT, E <: Tuple](mirror: Mirror.Product): ErasedProductInstances[K, FT] =
    inline erasedValue[Tuple.Size[E]] match {
      case 0 => ErasedProductInstancesN[K, FT](mirror, emptyArray)
      case 1 => ErasedProductInstances1[K, FT](mirror, summonOne[E])
      case _ => ErasedProductInstancesN[K, FT](mirror, summonAsArray[E])
    }
}

private[shapeless3] final class ErasedCoproductInstances[K, FT](mirror: Mirror.Sum, is0: => Array[Any]) extends ErasedInstances[K, FT] {
  lazy val is = is0

  final def erasedMapK(f: Any => Any): ErasedCoproductInstances[K, ?] =
    new ErasedCoproductInstances(mirror, is.map(f))

  final def ordinal(x: Any): Any = is(mirror.ordinal(x.asInstanceOf))

  final def erasedMap(x: Any)(f: (Any, Any) => Any): Any = {
    val i = ordinal(x)
    f(i, x)
  }

  final def erasedProject(p: Int)(i: Any)(f: (Any, Any) => (Any, Option[Any])): (Any, Option[Any]) =
    f(i, is(p))

  final def erasedInject(p: Int)(f: Any => Any): Any =
    f(is(p))

  final def erasedFold(x: Any)(f: (Any, Any) => Any): Any = {
    val i = ordinal(x)
    f(i, x)
  }

  final def erasedTraverse[F[_]](x: Any)(map: Map[F])(pure: Pure[F])(ap: Ap[F])(f: (Any, Any) => F[Any]): F[Any] =
    f(ordinal(x), x)

  final def erasedFold2(x: Any, y: Any)(a: => Any)(f: (Any, Any, Any) => Any): Any = {
    val i = mirror.ordinal(x.asInstanceOf)
    val j = mirror.ordinal(y.asInstanceOf)
    if(i == j) f(is(i), x, y)
    else a
  }

  final def erasedFold2f(x: Any, y: Any)(g: (Int, Int) => Any)(f: (Any, Any, Any) => Any): Any = {
    val i = mirror.ordinal(x.asInstanceOf)
    val j = mirror.ordinal(y.asInstanceOf)
    if(i == j) f(is(i), x, y)
    else g(i, j)
  }
}

private[shapeless3] object ErasedCoproductInstances {
  inline def apply[K, FT, E <: Tuple](mirror: Mirror.Sum) : ErasedCoproductInstances[K, FT] =
    new ErasedCoproductInstances[K, FT](mirror, summonAsArray[E])
}
