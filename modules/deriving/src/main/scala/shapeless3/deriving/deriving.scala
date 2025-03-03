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

import scala.annotation.implicitNotFound
import scala.collection.immutable.ArraySeq
import scala.compiletime.*
import scala.deriving.*
import scala.reflect.ClassTag

/** The identity type lambda. */
type Id[t] = t

/** A type lambda that returns a constant type. */
type Const[c] = [_] =>> c

/** Corresponds to `FunctionK` in Cats. */
infix type ~>[A[_], B[_]] = [t] => A[t] => B[t]

/** Corresponds to `Applicative.pure` in Cats. */
type Pure[F[_]] = [a] => a => F[a]

/** Corresponds to `Functor.map` in Cats. */
type MapF[F[_]] = [a, b] => (F[a], a => b) => F[b]

/** Corresponds to `Apply.ap` in Cats. */
type Ap[F[_]] = [a, b] => (F[a => b], F[a]) => F[b]

/** Corresponds to `FlatMap.tailRecM` in Cats. */
type TailRecM[F[_]] = [a, b] => (a, a => F[Either[a, b]]) => F[b]

/** Summon all elements from the tuple `T` as an array. */
inline def summonAsArray[T <: Tuple]: Array[Any] =
  summonAsArray0[T](0, new Array[Any](constValue[Tuple.Size[T]]))

/** Summon all elements from the tuple `T` into the provided array from index `i`. */
inline def summonAsArray0[T](i: Int, arr: Array[Any]): Array[Any] = inline erasedValue[T] match
  case _: EmptyTuple => arr
  case _: (a *: b) =>
    arr(i) = summonInline[a]
    summonAsArray0[b](i + 1, arr)

/** Summon all elements from the tuple `T` with a known upper bound as an array. */
inline def summonValuesAsArray[T <: Tuple, E: ClassTag]: Array[E] =
  summonValuesAsArray0[T, E](0, new Array[E](constValue[Tuple.Size[T]]))

/** Summon all elements from the tuple `T` with a known upper bound into the provided array from index `i`. */
inline def summonValuesAsArray0[T, E](i: Int, arr: Array[E]): Array[E] = inline erasedValue[T] match
  case _: EmptyTuple => arr
  case _: (a *: b) =>
    arr(i) = constValue[a & E]
    summonValuesAsArray0[b, E](i + 1, arr)

case class Labelling[T](label: String, elemLabels: IndexedSeq[String])
object Labelling:
  inline given apply[T](using mirror: Mirror { type MirroredType = T }): Labelling[T] =
    Labelling[T](
      constValue[mirror.MirroredLabel & String],
      ArraySeq.unsafeWrapArray(summonValuesAsArray[mirror.MirroredElemLabels, String])
    )

/** A type used to decide whether to continue folding over a product or short-circuit. */
type CompleteOr[T] = T | Complete[T]

/** Part of [[CompleteOr]] used to signal that folding should short-circuit. */
case class Complete[T](t: T)
object Complete:
  inline def apply[T](c: Boolean)(t: T)(f: T): CompleteOr[T] =
    if c then Complete(t) else f

/** Part of [[CompleteOr]] used to signal that folding should continue. */
object Continue:
  inline def apply[T](t: T): CompleteOr[T] = t

/** An opaque type wrapper useful for deriving third-party type classes. */
@implicitNotFound("Could not derive an instance of ${A}")
opaque type Derived[A] = A
object Derived:
  def apply[A](instance: A): Derived[A] = instance
  given [A]: Conversion[A, Derived[A]] = identity
  extension [A](derived: Derived[A]) def instance: A = derived
  extension [A](derived: OrElse[A, Derived[A]]) def unify: A = OrElse.unify(derived)

/** A type-level `orElse` - tries to summon `A` first and if not found, then `B`. */
opaque type OrElse[A, B] = A | B
object OrElse extends OrInstances:
  def apply[A, B](instance: A | B): OrElse[A, B] = instance
  given [A, B]: Conversion[OrElse[A, B], A | B] = identity
  extension [A, B](orElse: OrElse[A, B]) def unify: A | B = orElse

sealed abstract class OrInstances:
  inline given [A, B]: OrElse[A, B] = summonFrom:
    case instance: A => OrElse(instance)
    case instance: B => OrElse(instance)
