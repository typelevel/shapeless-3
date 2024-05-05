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

import shapeless3.deriving.internals.*

import scala.Tuple.Union
import scala.compiletime.*
import scala.compiletime.ops.int.S
import scala.deriving.*

trait Kind[Up <: AnyKind, Tup <: AnyKind, Mono[_ <: Up], Head[_ <: Tup] <: Up, Tail[_ <: Tup] <: Up]:
  self =>

  type of[M <: Mirror, O <: Up] = M {
    type MirroredType = O
    type MirroredMonoType = Mono[O]
    type MirroredElemTypes <: Tup
  }

  type Kind[M <: Mirror, O <: Up] = (M of O) { type Kind = self.type }
  type Generic[O <: Up] = Kind[Mirror, O]
  type ProductGeneric[O <: Up] = Kind[Mirror.Product, O]
  type CoproductGeneric[O <: Up] = Kind[Mirror.Sum, O]

  object Generic:
    given fromMirror[M <: Mirror, O <: Up](using m: M of O): Kind[m.type, O] = m.asInstanceOf

  def Generic[O <: Up](using gen: Generic[O]): gen.type = gen
  def ProductGeneric[O <: Up](using gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O <: Up](using gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_ <: Up], T <: Up] = ErasedInstances[self.type, F[T]]
  type ProductInstances[F[_ <: Up], T <: Up] = ErasedProductInstances[self.type, F[T]]
  type CoproductInstances[F[_ <: Up], T <: Up] = ErasedCoproductInstances[self.type, F[T]]

  type InstancesOf[F[_ <: Up]] = [T <: Up] =>> Instances[F, T]
  type ProductInstancesOf[F[_ <: Up]] = [T <: Up] =>> ProductInstances[F, T]
  type CoproductInstancesOf[F[_ <: Up]] = [T <: Up] =>> CoproductInstances[F, T]

  def Instances[F[_ <: Up], T <: Up](using inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_ <: Up], T <: Up](using inst: ProductInstances[F, T]): inst.type = inst
  def CoproductInstances[F[_ <: Up], T <: Up](using inst: CoproductInstances[F, T]): inst.type = inst

  type LiftP[F[_ <: Up], T <: Tup] <: Tuple = Mono[T] match
    case _ *: _ => F[Head[T]] *: LiftP[F, Tail[T]]
    case _ => EmptyTuple

  inline given mkInstances[F[_ <: Up], T <: Up](using gen: Mirror of T): Instances[F, T] = inline gen match
    case given (Mirror.Product of T) => mkProductInstances[F, T]
    case given (Mirror.Sum of T) => mkCoproductInstances[F, T]

  inline given mkProductInstances[F[_ <: Up], T <: Up](using gen: Mirror.Product of T): ProductInstances[F, T] =
    ErasedProductInstances[self.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_ <: Up], T <: Up](using gen: Mirror.Sum of T): CoproductInstances[F, T] =
    ErasedCoproductInstances[self.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  /**
   * Summon the first given instance `F[U]` from the tuple `T`. Remaining elements of `T` may or may not have an
   * instance of `F`.
   */
  inline def summonFirst[F[_ <: Up], T <: Tup]: F[Up] =
    Kinds.summonFirst[LiftP[F, T]].asInstanceOf

  /**
   * Summon the only given instance `F[U]` from the tuple `T`. Remaining elements of `T` are guaranteed to not have an
   * instance of `F`.
   */
  inline def summonOnly[F[_ <: Up], T <: Tup]: F[Up] =
    Kinds.summonOnly[LiftP[F, T]].asInstanceOf

  /** Ensure that no element of the tuple `T` has an instance of `F`. */
  inline def summonNone[F[_ <: Up], T <: Tup]: Unit =
    Kinds.summonNone[LiftP[F, T]]

  extension [F[_ <: Up], T <: Up](gen: Generic[T])
    inline def derive(
        f: => (ProductGeneric[T] & gen.type) ?=> F[T],
        g: => (CoproductGeneric[T] & gen.type) ?=> F[T]
    ): F[T] = inline gen match
      case p: ProductGeneric[T] => f(using p.asInstanceOf)
      case c: CoproductGeneric[T] => g(using c.asInstanceOf)

  extension [T <: Up](gen: CoproductGeneric[T])
    inline def withFirst[F[_ <: Up], R](f: [t <: T] => F[t] => R): R =
      f(summonFirst[F, gen.MirroredElemTypes].asInstanceOf)
    inline def withOnly[F[_ <: Up], R](f: [t <: T] => F[t] => R): R =
      f(summonOnly[F, gen.MirroredElemTypes].asInstanceOf)

  extension [F[_ <: Up], T <: Up](inst: Instances[F, T])
    inline def mapK[G[_ <: Up]](f: [t <: Up] => F[t] => G[t]): Instances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def widen[G[t <: Up] >: F[t]]: Instances[G, T] =
      inst.asInstanceOf

  extension [F[_ <: Up], T <: Up](inst: ProductInstances[F, T])
    inline def mapK[G[_ <: Up]](f: [t <: Up] => F[t] => G[t]): ProductInstances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def widen[G[t <: Up] >: F[t]]: ProductInstances[G, T] =
      inst.asInstanceOf

  extension [F[_ <: Up], T <: Up](inst: CoproductInstances[F, T])
    inline def mapK[G[_ <: Up]](f: [t <: Up] => F[t] => G[t]): CoproductInstances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def widen[G[t <: Up] >: F[t]]: CoproductInstances[G, T] =
      inst.asInstanceOf

object K0 extends Kind[Any, Tuple, Id, Kinds.Head, Kinds.Tail]:
  type IndexOf[E, X] = IndexOf0[E, X, 0]
  type IndexOf0[E, X, I <: Int] <: Int = X match
    case EmptyTuple => -1
    case x *: xs =>
      x match
        case E => I
        case _ => IndexOf0[E, xs, S[I]]

  @deprecated("Use summonFirst instead", "3.2.0")
  transparent inline def summonFirst0[T]: Any = Kinds.summonFirst[T]

  extension [T](gen: ProductGeneric[T])
    inline def toRepr(o: T): gen.MirroredElemTypes =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]
    inline def fromRepr(r: gen.MirroredElemTypes): T =
      gen.fromProduct(r.asInstanceOf)

  extension [T](gen: CoproductGeneric[T])
    inline def toRepr(o: T): Union[gen.MirroredElemTypes] = o.asInstanceOf
    inline def fromRepr(r: Union[gen.MirroredElemTypes]): T = r.asInstanceOf

  extension [F[_], T](inst: Instances[F, T])
    inline def map(x: T)(f: [t] => (F[t], t) => t): T =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf
    inline def traverse[G[_]](x: T)(map: MapF[G])(pure: Pure[G])(ap: Ap[G])(f: [t] => (F[t], t) => G[t]): G[T] =
      inst.erasedTraverse(x)(map)(pure)(ap)(f.asInstanceOf).asInstanceOf

  extension [F[_], T](inst: ProductInstances[F, T])
    inline def construct(f: [t] => F[t] => t): T =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def constructA[G[_]](f: [t] => F[t] => G[t])(pure: Pure[G], map: MapF[G], ap: Ap[G]): G[T] =
      inst.erasedConstructA(f.asInstanceOf)(pure, map, ap).asInstanceOf
    inline def constructM[G[_]](f: [t] => F[t] => G[t])(pure: Pure[G], map: MapF[G], tailRecM: TailRecM[G]): G[T] =
      inst.erasedConstructM(f.asInstanceOf)(pure, map, tailRecM).asInstanceOf
    inline def map2(x: T, y: T)(f: [t] => (F[t], t, t) => t): T =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def unfold[Acc](i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
      inst.erasedUnfold(i)(f.asInstanceOf).asInstanceOf
    inline def foldLeft[Acc](x: T)(i: Acc)(f: [t] => (Acc, F[t], t) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldLeft2[Acc](x: T, y: T)(i: Acc)(f: [t] => (Acc, F[t], t, t) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight[Acc](x: T)(i: Acc)(f: [t] => (F[t], t, Acc) => CompleteOr[Acc]): Acc =
      inst.erasedFoldRight(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight2[Acc](x: T, y: T)(i: Acc)(f: [t] => (F[t], t, t, Acc) => CompleteOr[Acc]): Acc =
      inst.erasedFoldRight2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def project[R](t: T)(p: Int)(f: [t] => (F[t], t) => R): R =
      inst.erasedProject(t)(p)(f.asInstanceOf).asInstanceOf

  extension [F[_], T](inst: CoproductInstances[F, T])
    inline def inject[R](p: Int)(f: [t <: T] => F[t] => R): R =
      inst.erasedInject(p)(f.asInstanceOf).asInstanceOf
    @deprecated("use inject", "3.0.2")
    inline def project[Acc](p: Int)(i: Acc)(f: [t] => (Acc, F[t]) => (Acc, Option[t])): (Acc, Option[T]) =
      inst.erasedProject(p)(i)(f.asInstanceOf).asInstanceOf
    inline def fold[R](x: T)(f: [t <: T] => (F[t], t) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def fold2[R](x: T, y: T)(a: => R)(f: [t <: T] => (F[t], t, t) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def fold2[R](x: T, y: T)(g: (Int, Int) => R)(f: [t <: T] => (F[t], t, t) => R): R =
      inst.erasedFold2f(x, y)(g.asInstanceOf)(f.asInstanceOf).asInstanceOf

object K1
    extends Kind[
      [_] =>> Any,
      [_] =>> Tuple,
      [t[_]] =>> t[Any],
      [t[_]] =>> [a] =>> Kinds.Head[t[a]],
      [t[_]] =>> [a] =>> Kinds.Tail[t[a]]
    ]:

  @deprecated("Use summonFirst instead", "3.2.0")
  transparent inline def summonFirst0[T]: Any = Kinds.summonFirst[T]

  extension [T[_], A](gen: ProductGeneric[T])
    inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
    inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] =
      gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

  extension [T[_], A](gen: CoproductGeneric[T])
    inline def toRepr(o: T[A]): Union[gen.MirroredElemTypes[A]] = o.asInstanceOf
    inline def fromRepr(r: Union[gen.MirroredElemTypes[A]]): T[A] = r.asInstanceOf

  extension [F[_[_]], T[_]](inst: Instances[F, T])
    inline def map[A, R](x: T[A])(f: [t[_]] => (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf
    inline def traverse[A, G[_], R](x: T[A])(map: MapF[G])(pure: Pure[G])(ap: Ap[G])(
        f: [t[_]] => (F[t], t[A]) => G[t[R]]
    ): G[T[R]] =
      inst.erasedTraverse(x)(map)(pure)(ap)(f.asInstanceOf).asInstanceOf

  extension [F[_[_]], T[_]](inst: ProductInstances[F, T])
    inline def construct[R](f: [t[_]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def constructA[G[_], R](f: [t[_]] => F[t] => G[t[R]])(pure: Pure[G], map: MapF[G], ap: Ap[G]): G[T[R]] =
      inst.erasedConstructA(f.asInstanceOf)(pure, map, ap).asInstanceOf
    inline def constructM[G[_], R](
        f: [t[_]] => F[t] => G[t[R]]
    )(pure: Pure[G], map: MapF[G], tailRecM: TailRecM[G]): G[T[R]] =
      inst.erasedConstructM(f.asInstanceOf)(pure, map, tailRecM).asInstanceOf
    inline def map2[A, B, R](x: T[A], y: T[B])(f: [t[_]] => (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def foldLeft[A, Acc](x: T[A])(i: Acc)(f: [t[_]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldLeft2[A, B, Acc](x: T[A], y: T[B])(i: Acc)(
        f: [t[_]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight[A, Acc](x: T[A])(i: Acc)(f: [t[_]] => (F[t], t[A], Acc) => CompleteOr[Acc]): Acc =
      inst.erasedFoldRight(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight2[A, B, Acc](x: T[A], y: T[B])(i: Acc)(
        f: [t[_]] => (F[t], t[A], t[B], Acc) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldRight2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def project[A, R](t: T[A])(p: Int)(f: [t[_]] => (F[t], t[A]) => R): R =
      inst.erasedProject(t)(p)(f.asInstanceOf).asInstanceOf

  extension [F[_[_]], T[_]](inst: CoproductInstances[F, T])
    inline def fold[A, R](x: T[A])(f: [t[x] <: T[x]] => (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def fold2[A, B, R](x: T[A], y: T[B])(a: => R)(f: [t[x] <: T[x]] => (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def fold2[A, B, R](x: T[A], y: T[B])(g: (Int, Int) => R)(f: [t[x] <: T[x]] => (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2f(x, y)(g.asInstanceOf)(f.asInstanceOf).asInstanceOf

object K11
    extends Kind[
      [_[_]] =>> Any,
      [_[_]] =>> Tuple,
      [t[_[_]]] =>> t[[_] =>> Any],
      [t[_[_]]] =>> [a[_]] =>> Kinds.Head[t[a]],
      [t[_[_]]] =>> [a[_]] =>> Kinds.Tail[t[a]]
    ]:

  type Id[t] = [f[_]] =>> f[t]
  type Const[c] = [f[_]] =>> c

  extension [T[_[_]], A[_]](gen: ProductGeneric[T])
    inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
    inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] =
      gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

  extension [T[_[_]], A[_]](gen: CoproductGeneric[T])
    inline def toRepr(o: T[A]): Union[gen.MirroredElemTypes[A]] = o.asInstanceOf
    inline def fromRepr(r: Union[gen.MirroredElemTypes[A]]): T[A] = r.asInstanceOf

  extension [F[_[_[_]]], T[_[_]]](inst: Instances[F, T])
    inline def map[A[_], R[_]](x: T[A])(f: [t[_[_]]] => (F[t], t[A]) => t[R]): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf
    inline def traverse[A[_], G[_], R[_]](x: T[A])(map: MapF[G])(pure: Pure[G])(ap: Ap[G])(
        f: [t[_[_]]] => (F[t], t[A]) => G[t[R]]
    ): G[T[R]] =
      inst.erasedTraverse(x)(map)(pure)(ap)(f.asInstanceOf).asInstanceOf

  extension [F[_[_[_]]], T[_[_]]](inst: ProductInstances[F, T])
    inline def construct[R[_]](f: [t[_[_]]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def constructA[G[_], R[_]](
        f: [t[_[_]]] => F[t] => G[t[R]]
    )(pure: Pure[G], map: MapF[G], ap: Ap[G]): G[T[R]] =
      inst.erasedConstructA(f.asInstanceOf)(pure, map, ap).asInstanceOf
    inline def constructM[G[_], R[_]](
        f: [t[_[_]]] => F[t] => G[t[R]]
    )(pure: Pure[G], map: MapF[G], tailRecM: TailRecM[G]): G[T[R]] =
      inst.erasedConstructM(f.asInstanceOf)(pure, map, tailRecM).asInstanceOf
    inline def map2[A[_], B[_], R[_]](x: T[A], y: T[B])(f: [t[_[_]]] => (F[t], t[A], t[B]) => t[R]): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def foldLeft[A[_], Acc](x: T[A])(i: Acc)(f: [t[_[_]]] => (Acc, F[t], t[A]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldLeft2[A[_], B[_], Acc](x: T[A], y: T[B])(i: Acc)(
        f: [t[_[_]]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight[A[_], Acc](x: T[A])(i: Acc)(f: [t[_[_]]] => (F[t], t[A], Acc) => CompleteOr[Acc]): Acc =
      inst.erasedFoldRight(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight2[A[_], B[_], Acc](x: T[A], y: T[B])(i: Acc)(
        f: [t[_[_]]] => (F[t], t[A], t[B], Acc) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldRight2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def project[A[_], R](t: T[A])(p: Int)(f: [t[_[_]]] => (F[t], t[A]) => R): R =
      inst.erasedProject(t)(p)(f.asInstanceOf).asInstanceOf

  extension [F[_[_[_]]], T[_[_]]](inst: CoproductInstances[F, T])
    inline def fold[A[_], R](x: T[A])(f: [t[x[_]] <: T[x]] => (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def fold2[A[_], B[_], R](x: T[A], y: T[B])(a: => R)(f: [t[x[_]] <: T[x]] => (F[t], t[A], t[B]) => R): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def fold2[A[_], B[_], R](x: T[A], y: T[B])(g: (Int, Int) => R)(
        f: [t[x[_]] <: T[x]] => (F[t], t[A], t[B]) => R
    ): R =
      inst.erasedFold2f(x, y)(g.asInstanceOf)(f.asInstanceOf).asInstanceOf

object K2
    extends Kind[
      [_, _] =>> Any,
      [_, _] =>> Tuple,
      [t[_, _]] =>> t[Any, Any],
      [t[_, _]] =>> [a, b] =>> Kinds.Head[t[a, b]],
      [t[_, _]] =>> [a, b] =>> Kinds.Tail[t[a, b]]
    ]:

  type Id1[t, u] = t
  type Id2[t, u] = u
  type Const[c] = [t, u] =>> c

  @deprecated("Use summonFirst instead", "3.2.0")
  transparent inline def summonFirst0[T]: Any = Kinds.summonFirst[T]

  extension [T[_, _], A, B](gen: ProductGeneric[T])
    inline def toRepr(o: T[A, B]): gen.MirroredElemTypes[A, B] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A, B]]
    inline def fromRepr(r: gen.MirroredElemTypes[A, B]): T[A, B] =
      gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A, B]]

  extension [T[_, _], A, B](gen: CoproductGeneric[T])
    inline def toRepr(o: T[A, B]): Union[gen.MirroredElemTypes[A, B]] = o.asInstanceOf
    inline def fromRepr(r: Union[gen.MirroredElemTypes[A, B]]): T[A, B] = r.asInstanceOf

  extension [F[_[_, _]], T[_, _]](inst: Instances[F, T])
    inline def map[A, B, R, S](x: T[A, B])(f: [t[_, _]] => (F[t], t[A, B]) => t[R, S]): T[R, S] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf
    inline def traverse[A, B, G[_], R, S](x: T[A, B])(map: MapF[G])(pure: Pure[G])(ap: Ap[G])(
        f: [t[_, _]] => (F[t], t[A, B]) => G[t[R, S]]
    ): G[T[R, S]] =
      inst.erasedTraverse(x)(map)(pure)(ap)(f.asInstanceOf).asInstanceOf

  extension [F[_[_, _]], T[_, _]](inst: ProductInstances[F, T])
    inline def construct[R, S](f: [t[_, _]] => F[t] => t[R, S]): T[R, S] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def constructA[G[_], R, S](
        f: [t[_, _]] => F[t] => G[t[R, S]]
    )(pure: Pure[G], map: MapF[G], ap: Ap[G]): G[T[R, S]] =
      inst.erasedConstructA(f.asInstanceOf)(pure, map, ap).asInstanceOf
    inline def constructM[G[_], R, S](
        f: [t[_, _]] => F[t] => G[t[R, S]]
    )(pure: Pure[G], map: MapF[G], tailRecM: TailRecM[G]): G[T[R, S]] =
      inst.erasedConstructM(f.asInstanceOf)(pure, map, tailRecM).asInstanceOf
    inline def map2[A, B, C, D, R, S](x: T[A, B], y: T[C, D])(
        f: [t[_, _]] => (F[t], t[A, B], t[C, D]) => t[R, S]
    ): T[R, S] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def foldLeft[A, B, Acc](x: T[A, B])(i: Acc)(f: [t[_, _]] => (Acc, F[t], t[A, B]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldLeft2[A, B, C, D, Acc](x: T[A, B], y: T[C, D])(i: Acc)(
        f: [t[_, _]] => (Acc, F[t], t[A, B], t[C, D]) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight[A, B, Acc](x: T[A, B])(i: Acc)(f: [t[_, _]] => (F[t], t[A, B], Acc) => CompleteOr[Acc]): Acc =
      inst.erasedFoldRight(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight2[A, B, C, D, Acc](x: T[A, B], y: T[C, D])(i: Acc)(
        f: [t[_, _]] => (F[t], t[A, B], t[C, D], Acc) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldRight2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def project[A, B, R](t: T[A, B])(p: Int)(f: [t[_, _]] => (F[t], t[A, B]) => R): R =
      inst.erasedProject(t)(p)(f.asInstanceOf).asInstanceOf

  extension [F[_[_, _]], T[_, _]](inst: CoproductInstances[F, T])
    inline def fold[A, B, R](x: T[A, B])(f: [t[x, y] <: T[x, y]] => (F[t], t[A, B]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def fold2[A, B, C, D, R](x: T[A, B], y: T[C, D])(a: => R)(
        f: [t[x, y] <: T[x, y]] => (F[t], t[A, B], t[C, D]) => R
    ): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def fold2[A, B, C, D, R](x: T[A, B], y: T[C, D])(g: (Int, Int) => R)(
        f: [t[x, y] <: T[x, y]] => (F[t], t[A, B], t[C, D]) => R
    ): R =
      inst.erasedFold2f(x, y)(g.asInstanceOf)(f.asInstanceOf).asInstanceOf

object K21
    extends Kind[
      [_[_], _[_]] =>> Any,
      [_[_], _[_]] =>> Tuple,
      [t[_[_], _[_]]] =>> t[[_] =>> Any, [_] =>> Any],
      [t[_[_], _[_]]] =>> [a[_], b[_]] =>> Kinds.Head[t[a, b]],
      [t[_[_], _[_]]] =>> [a[_], b[_]] =>> Kinds.Tail[t[a, b]]
    ]:

  extension [T[_[_], _[_]], A[_], B[_]](gen: ProductGeneric[T])
    inline def toRepr(o: T[A, B]): gen.MirroredElemTypes[A, B] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A, B]]
    inline def fromRepr(r: gen.MirroredElemTypes[A, B]): T[A, B] =
      gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A, B]]

  extension [T[_[_], _[_]], A[_], B[_]](gen: CoproductGeneric[T])
    inline def toRepr(o: T[A, B]): Union[gen.MirroredElemTypes[A, B]] = o.asInstanceOf
    inline def fromRepr(r: Union[gen.MirroredElemTypes[A, B]]): T[A, B] = r.asInstanceOf

  extension [F[_[_[_], _[_]]], T[_[_], _[_]]](inst: Instances[F, T])
    inline def map[A[_], B[_], R[_], S[_]](x: T[A, B])(f: [t[_[_], _[_]]] => (F[t], t[A, B]) => t[R, S]): T[R, S] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf
    inline def traverse[A[_], B[_], G[_], R[_], S[_]](x: T[A, B])(map: MapF[G])(pure: Pure[G])(ap: Ap[G])(
        f: [t[_[_], _[_]]] => (F[t], t[A, B]) => G[t[R, S]]
    ): G[T[R, S]] =
      inst.erasedTraverse(x)(map)(pure)(ap)(f.asInstanceOf).asInstanceOf

  extension [F[_[_[_], _[_]]], T[_[_], _[_]]](inst: ProductInstances[F, T])
    inline def construct[R[_], S[_]](f: [t[_[_], _[_]]] => F[t] => t[R, S]): T[R, S] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def constructA[G[_], R[_], S[_]](
        f: [t[_[_], _[_]]] => F[t] => G[t[R, S]]
    )(pure: Pure[G], map: MapF[G], ap: Ap[G]): G[T[R, S]] =
      inst.erasedConstructA(f.asInstanceOf)(pure, map, ap).asInstanceOf
    inline def constructM[G[_], R[_], S[_]](
        f: [t[_[_], _[_]]] => F[t] => G[t[R, S]]
    )(pure: Pure[G], map: MapF[G], tailRecM: TailRecM[G]): G[T[R, S]] =
      inst.erasedConstructM(f.asInstanceOf)(pure, map, tailRecM).asInstanceOf
    inline def map2[A[_], B[_], C[_], D[_], R[_], S[_]](x: T[A, B], y: T[C, D])(
        f: [t[_[_], _[_]]] => (F[t], t[A, B], t[B, C]) => t[R, S]
    ): T[R, S] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def foldLeft[A[_], B[_], Acc](x: T[A, B])(i: Acc)(
        f: [t[_[_], _[_]]] => (Acc, F[t], t[A, B]) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldLeft2[A[_], B[_], C[_], D[_], Acc](x: T[A, B], y: T[C, D])(i: Acc)(
        f: [t[_[_], _[_]]] => (Acc, F[t], t[A, B], t[C, D]) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight[A[_], B[_], Acc](x: T[A, B])(i: Acc)(
        f: [t[_[_], _[_]]] => (F[t], t[A, B], Acc) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldRight(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight2[A[_], B[_], C[_], D[_], Acc](x: T[A, B], y: T[C, D])(i: Acc)(
        f: [t[_[_], _[_]]] => (F[t], t[A, B], t[C, D], Acc) => CompleteOr[Acc]
    ): Acc =
      inst.erasedFoldRight2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def project[A[_], B[_], R](t: T[A, B])(p: Int)(f: [t[_[_], _[_]]] => (F[t], t[A, B]) => R): R =
      inst.erasedProject(t)(p)(f.asInstanceOf).asInstanceOf

  extension [F[_[_[_], _[_]]], T[_[_], _[_]]](inst: CoproductInstances[F, T])
    inline def fold[A[_], B[_], R](x: T[A, B])(f: [t[a[_], b[_]] <: T[a, b]] => (F[t], t[A, B]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def fold2[A[_], B[_], C[_], D[_], R](x: T[A, B], y: T[C, D])(a: => R)(
        f: [t[a[_], b[_]] <: T[a, b]] => (F[t], t[A, B], t[C, D]) => R
    ): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def fold2[A[_], B[_], C[_], D[_], R](x: T[A, B], y: T[C, D])(g: (Int, Int) => R)(
        f: [t[a[_], b[_]] <: T[a, b]] => (F[t], t[A, B], t[C, D]) => R
    ): R =
      inst.erasedFold2f(x, y)(g.asInstanceOf)(f.asInstanceOf).asInstanceOf
