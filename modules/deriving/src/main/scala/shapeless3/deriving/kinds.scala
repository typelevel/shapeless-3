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

/**
 * A helper trait for specifying kinds supported by Shapeless.
 *
 * @see
 *   [[K0]], [[K1]], [[K11]], [[K2]] for examples how to use this trait.
 *
 * @tparam Up
 *   The upper bound of this kind. Use a type lambda to specify higher-kinds.
 * @tparam Tup
 *   The upper bound of tuples with this kind. Should have the same kind as `Up` but result in a `Tuple`.
 * @tparam Mono
 *   A type lambda that monomorphizes a type with kind `Up` - it applies it to the upper bounds of its type parameters.
 * @tparam Head
 *   A type lambda that returns the head type of a tuple with kind `Tup`.
 * @tparam Tail
 *   A type lambda that returns the tail type of a tuple with kind `Tup`.
 *
 * @define productToRepr
 *   Convert a product value to its equivalent representation as a typed tuple.
 *
 * @define coproductToRepr
 *   Convert a sum value to its equivalent representation as a union type. Noop at runtime.
 *
 * @define productFromRepr
 *   Construct a product value from its equivalent representation of a typed tuple.
 *
 * @define coproductFromRepr
 *   Construct a sum value from its equivalent representation of a union type. Noop at runtime.
 */
trait Kind[Up <: AnyKind, Tup <: AnyKind, Mono[_ <: Up], Head[_ <: Tup] <: Up, Tail[_ <: Tup] <: Up]:
  self =>

  /** Similar to [[Mirror.Of]] but generalized to this kind. */
  infix type of[M <: Mirror, O <: Up] = M {
    type MirroredType = O
    type MirroredMonoType = Mono[O]
    type MirroredElemTypes <: Tup
  }

  /** A [[Mirror]] for types of this kind, including the given scope of the enclosing object. */
  type Kind[M <: Mirror, O <: Up] = (M of O) { type Kind = self.type }

  /** A [[Mirror]] for types of this kind. */
  type Generic[O <: Up] = Kind[Mirror, O]

  /** A [[Mirror.Product]] for types of this kind. */
  type ProductGeneric[O <: Up] = Kind[Mirror.Product, O]

  /** A [[Mirror.Sum]] for types of this kind. */
  type CoproductGeneric[O <: Up] = Kind[Mirror.Sum, O]

  object Generic:

    /**
     * Attaches the given scope of the enclosing object to a given [[Mirror]] as a type refinement.
     *
     * The Scala compiler can automatically add type refinements to synthesized mirrors, but not to given mirrors in the
     * current scope or to user-defined mirrors. The scope of the enclosing object is necessary to resolve extension
     * methods defined on mirrors by Shapeless without an import.
     */
    given fromMirror[M <: Mirror, O <: Up](using m: M of O): Kind[m.type, O] = m.asInstanceOf

  /** Summon or synthesize [[Generic]]. */
  def Generic[O <: Up](using gen: Generic[O]): gen.type = gen

  /** Summon or synthesize [[ProductGeneric]]. */
  def ProductGeneric[O <: Up](using gen: ProductGeneric[O]): gen.type = gen

  /** Summon or synthesize [[CoproductGeneric]]. */
  def CoproductGeneric[O <: Up](using gen: CoproductGeneric[O]): gen.type = gen

  /** Resolved instances of the type class `F` for all fields or variants of the type `T`. */
  type Instances[F[_ <: Up], T <: Up] = ErasedInstances[self.type, F[T]]

  /** Resolved instances of the type class `F` for all fields of the product type `T`. */
  type ProductInstances[F[_ <: Up], T <: Up] = ErasedProductInstances[self.type, F[T]]

  /** Resolved instances of the type class `F` for all variants of the sum type `T`. */
  type CoproductInstances[F[_ <: Up], T <: Up] = ErasedCoproductInstances[self.type, F[T]]

  /** A curried version of [[Instances]] that can be used as a context bound. */
  type InstancesOf[F[_ <: Up]] = [T <: Up] =>> Instances[F, T]

  /** A curried version of [[ProductInstances]] that can be used as a context bound. */
  type ProductInstancesOf[F[_ <: Up]] = [T <: Up] =>> ProductInstances[F, T]

  /** A curried version of [[CoproductInstances]] that can be used as a context bound. */
  type CoproductInstancesOf[F[_ <: Up]] = [T <: Up] =>> CoproductInstances[F, T]

  /** Summon instances of the type class `F` for all fields or variants of the type `T`. */
  def Instances[F[_ <: Up], T <: Up](using inst: Instances[F, T]): inst.type = inst

  /** Summon instances of the type class `F` for all fields of the product type `T`. */
  def ProductInstances[F[_ <: Up], T <: Up](using inst: ProductInstances[F, T]): inst.type = inst

  /** Summon instances of the type class `F` for all variants of the sum type `T`. */
  def CoproductInstances[F[_ <: Up], T <: Up](using inst: CoproductInstances[F, T]): inst.type = inst

  /** Applies `F` to all elements of the tuple type `T`. Similar to [[Tuple.Map]] but generalized to this kind. */
  type LiftP[F[_ <: Up], T <: Tup] <: Tuple = Mono[T] match
    case _ *: _ => F[Head[T]] *: LiftP[F, Tail[T]]
    case _ => EmptyTuple

  /** Given a [[Mirror]], provides instances of the type class `F` for all fields or variants of `T`. */
  inline given mkInstances[F[_ <: Up], T <: Up](using gen: Mirror of T): Instances[F, T] = inline gen match
    case given (Mirror.Product of T) => mkProductInstances[F, T]
    case given (Mirror.Sum of T) => mkCoproductInstances[F, T]

  /** Given a [[Mirror.Product]], provides instances of the type class `F` for all fields of `T`. */
  inline given mkProductInstances[F[_ <: Up], T <: Up](using gen: Mirror.Product of T): ProductInstances[F, T] =
    ErasedProductInstances[self.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  /** Given a [[Mirror.Sum]], provides instances of the type class `F` for all variants of `T`. */
  inline given mkCoproductInstances[F[_ <: Up], T <: Up](using gen: Mirror.Sum of T): CoproductInstances[F, T] =
    ErasedCoproductInstances[self.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  /**
   * Summon the first given instance of `F` from the tuple type `T`. Remaining elements of `T` may or may not have an
   * instance of `F`.
   */
  inline def summonFirst[F[_ <: Up], T <: Tup]: F[Up] =
    Kinds.summonFirst[LiftP[F, T]].asInstanceOf

  /**
   * Summon the only given instance of `F` from the tuple type `T`. Remaining elements of `T` are guaranteed to not have
   * an instance of `F` in scope.
   */
  inline def summonOnly[F[_ <: Up], T <: Tup]: F[Up] =
    Kinds.summonOnly[LiftP[F, T]].asInstanceOf

  /** Ensure that no element of the tuple type `T` has an instance of `F`. */
  inline def summonNone[F[_ <: Up], T <: Tup]: Unit =
    Kinds.summonNone[LiftP[F, T]]

  extension [F[_ <: Up], T <: Up](gen: Generic[T])
    /** Derive an instance of `F[T]` depending on whether `T` is a product or sum type. */
    inline def derive(
        f: => (ProductGeneric[T] & gen.type) ?=> F[T],
        g: => (CoproductGeneric[T] & gen.type) ?=> F[T]
    ): F[T] = inline gen match
      case p: ProductGeneric[T] => f(using p.asInstanceOf)
      case c: CoproductGeneric[T] => g(using c.asInstanceOf)

  extension [T <: Up](gen: CoproductGeneric[T])
    /** Use the first given instance of `F` among the variants of the sum type `T` to produce a result. */
    inline def withFirst[F[_ <: Up], R](f: [t <: T] => F[t] => R): R =
      f(summonFirst[F, gen.MirroredElemTypes].asInstanceOf)

    /** Use the only given instance of `F` among the variants of the sum type `T` to produce a result. */
    inline def withOnly[F[_ <: Up], R](f: [t <: T] => F[t] => R): R =
      f(summonOnly[F, gen.MirroredElemTypes].asInstanceOf)

  extension [F[_ <: Up], T <: Up](inst: Instances[F, T])
    /** Transform the type class `F` in these instances to `G` by applying a polymorphic function. */
    inline def mapK[G[_ <: Up]](f: [t <: Up] => F[t] => G[t]): Instances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf

    /** Widen the type class `F` in these instances to a super type `G`. Noop at runtime. */
    inline def widen[G[t <: Up] >: F[t]]: Instances[G, T] = inst.asInstanceOf

  extension [F[_ <: Up], T <: Up](inst: ProductInstances[F, T])
    /** Transform the type class `F` in these product instances to `G` by applying a polymorphic function. */
    inline def mapK[G[_ <: Up]](f: [t <: Up] => F[t] => G[t]): ProductInstances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf

    /** Widen the type class `F` in these product instances to a super type `G`. Noop at runtime. */
    inline def widen[G[t <: Up] >: F[t]]: ProductInstances[G, T] = inst.asInstanceOf

  extension [F[_ <: Up], T <: Up](inst: CoproductInstances[F, T])
    /** Transform the type class `F` in these coproduct instances to `G` by applying a polymorphic function. */
    inline def mapK[G[_ <: Up]](f: [t <: Up] => F[t] => G[t]): CoproductInstances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf

    /** Widen the type class `F` in these coproduct instances to a super type `G`. Noop at runtime. */
    inline def widen[G[t <: Up] >: F[t]]: CoproductInstances[G, T] = inst.asInstanceOf

object K0 extends Kind[Any, Tuple, Id, Kinds.Head, Kinds.Tail]:
  /** Returns the index of element type `E` in the tuple type `T` as a literal type, `-1` if `E` is not in `T`. */
  type IndexOf[E, T] = IndexOf0[E, T, 0]
  type IndexOf0[E, T, I <: Int] <: Int = T match
    case EmptyTuple => -1
    case x *: xs =>
      x match
        case E => I
        case _ => IndexOf0[E, xs, S[I]]

  @deprecated("Use summonFirst instead", "3.2.0")
  transparent inline def summonFirst0[T <: Tuple]: Any = Kinds.summonFirst[T]

  extension [T](gen: ProductGeneric[T])
    /** $productToRepr */
    inline def toRepr(o: T): gen.MirroredElemTypes =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes]

    /** $productFromRepr */
    inline def fromRepr(r: gen.MirroredElemTypes): T =
      gen.fromProduct(r)

  extension [T](gen: CoproductGeneric[T])
    /** $coproductToRepr */
    inline def toRepr(o: T): Union[gen.MirroredElemTypes] = o.asInstanceOf

    /** $coproductFromRepr */
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
    inline def foldLeft0[Acc](i: Acc)(f: [t] => (Acc, F[t]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft0(i)(f.asInstanceOf).asInstanceOf
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
  transparent inline def summonFirst0[T <: Tuple]: Any = Kinds.summonFirst[T]

  extension [T[_], A](gen: ProductGeneric[T])
    /** $productToRepr */
    inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]

    /** $productFromRepr */
    inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] =
      gen.fromProduct(r).asInstanceOf[T[A]]

  extension [T[_], A](gen: CoproductGeneric[T])
    /** $coproductToRepr */
    inline def toRepr(o: T[A]): Union[gen.MirroredElemTypes[A]] = o.asInstanceOf

    /** $coproductFromRepr */
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
    inline def foldLeft0[Acc](i: Acc)(f: [t[_]] => (Acc, F[t]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft0(i)(f.asInstanceOf).asInstanceOf
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
    /** $productToRepr */
    inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]

    /** $productFromRepr */
    inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] =
      gen.fromProduct(r).asInstanceOf[T[A]]

  extension [T[_[_]], A[_]](gen: CoproductGeneric[T])
    /** $coproductToRepr */
    inline def toRepr(o: T[A]): Union[gen.MirroredElemTypes[A]] = o.asInstanceOf

    /** $coproductFromRepr */
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
    inline def foldLeft0[Acc](i: Acc)(f: [t[_[_]]] => (Acc, F[t]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft0(i)(f.asInstanceOf).asInstanceOf
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
  transparent inline def summonFirst0[T <: Tuple]: Any = Kinds.summonFirst[T]

  extension [T[_, _], A, B](gen: ProductGeneric[T])
    /** $productToRepr */
    inline def toRepr(o: T[A, B]): gen.MirroredElemTypes[A, B] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A, B]]

    /** $productFromRepr */
    inline def fromRepr(r: gen.MirroredElemTypes[A, B]): T[A, B] =
      gen.fromProduct(r).asInstanceOf[T[A, B]]

  extension [T[_, _], A, B](gen: CoproductGeneric[T])
    /** $coproductToRepr */
    inline def toRepr(o: T[A, B]): Union[gen.MirroredElemTypes[A, B]] = o.asInstanceOf

    /** $coproductFromRepr */
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
    inline def foldLeft0[Acc](i: Acc)(f: [t[_, _]] => (Acc, F[t]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft0(i)(f.asInstanceOf).asInstanceOf
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
