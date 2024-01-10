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

import scala.annotation.tailrec
import scala.compiletime.*

import cats.Eval

// Type classes

trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

object Monoid:
  inline def apply[A](using ma: Monoid[A]): Monoid[A] = ma

  given Monoid[Unit] with
    def empty: Unit = ()
    def combine(x: Unit, y: Unit): Unit = ()

  given Monoid[Boolean] with
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y

  given Monoid[Int] with
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y

  given Monoid[String] with
    def empty: String = ""
    def combine(x: String, y: String): String = x + y

  given [A](using M: Monoid[A]): Monoid[Option[A]] with
    def empty: Option[A] = None

    def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match
      case x -> None => x
      case None -> y => y
      case Some(x) -> Some(y) => Some(M.combine(x, y))

  given monoidGen[A](using inst: K0.ProductInstances[Monoid, A]): Monoid[A] with
    def empty: A = inst.construct([t] => (ma: Monoid[t]) => ma.empty)
    def combine(x: A, y: A): A = inst.map2(x, y)([t] => (mt: Monoid[t], t0: t, t1: t) => mt.combine(t0, t1))

  inline def derived[A](using gen: K0.ProductGeneric[A]): Monoid[A] = monoidGen

trait Eq[A]:
  def eqv(x: A, y: A): Boolean

object Eq:
  inline def apply[A](using ea: Eq[A]): Eq[A] = ea

  given Eq[Unit] with
    def eqv(x: Unit, y: Unit): Boolean = true

  given Eq[Boolean] with
    def eqv(x: Boolean, y: Boolean): Boolean = x == y

  given Eq[Int] with
    def eqv(x: Int, y: Int): Boolean = x == y

  given Eq[String] with
    def eqv(x: String, y: String): Boolean = x == y

  given eqGen[A](using inst: K0.ProductInstances[Eq, A]): Eq[A] with
    def eqv(x: A, y: A): Boolean = inst.foldLeft2(x, y)(true: Boolean)(
      [t] => (acc: Boolean, eqt: Eq[t], t0: t, t1: t) => Complete(!eqt.eqv(t0, t1))(false)(true)
    )

  given eqGenC[A](using inst: K0.CoproductInstances[Eq, A]): Eq[A] with
    def eqv(x: A, y: A): Boolean = inst.fold2(x, y)(false)(
      [t] => (eqt: Eq[t], t0: t, t1: t) => eqt.eqv(t0, t1)
    )

  inline def derived[A](using gen: K0.Generic[A]): Eq[A] =
    gen.derive(eqGen, eqGenC)

trait Ord[A] extends Eq[A]:
  def eqv(x: A, y: A): Boolean = compare(x, y) == 0

  def compare(x: A, y: A): Int

object Ord:
  inline def apply[A](using oa: Ord[A]): oa.type = oa

  given Ord[Unit] with
    override def eqv(x: Unit, y: Unit): Boolean = true

    def compare(x: Unit, y: Unit): Int = 0

  given Ord[Boolean] with
    override def eqv(x: Boolean, y: Boolean) = x == y

    def compare(x: Boolean, y: Boolean): Int =
      if x == y then 0 else if x then 1 else -1

  given Ord[Int] with
    override def eqv(x: Int, y: Int): Boolean = x == y

    def compare(x: Int, y: Int): Int = x - y

  given Ord[String] with
    override def eqv(x: String, y: String): Boolean = x == y

    def compare(x: String, y: String): Int = x.compare(y)

  given ordGen[A](using inst: K0.ProductInstances[Ord, A]): Ord[A] with
    def compare(x: A, y: A): Int = inst.foldLeft2(x, y)(0: Int)(
      [t] =>
        (acc: Int, ord: Ord[t], t0: t, t1: t) =>
          val cmp = ord.compare(t0, t1)
          Complete(cmp != 0)(cmp)(acc)
    )

  given ordGenC[A](using inst: K0.CoproductInstances[Ord, A]): Ord[A] with
    def compare(x: A, y: A): Int = inst.fold2(x, y)((x: Int, y: Int) => x - y)(
      [t] => (ord: Ord[t], t0: t, t1: t) => ord.compare(t0, t1)
    )

  inline def derived[A](using gen: K0.Generic[A]): Ord[A] =
    gen.derive(ordGen, ordGenC)

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

object Functor:
  inline def apply[F[_]](using ff: Functor[F]): Functor[F] = ff

  given Functor[Id] with
    def map[A, B](a: A)(f: A => B): B = f(a)

  given [F[_], G[_]](using ff: Functor[F], fg: Functor[G]): Functor[[t] =>> F[G[t]]] with
    def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = ff.map(fga)(ga => fg.map(ga)(f))

  given functorGen[F[_]](using inst: K1.Instances[Functor, F]): Functor[F] with
    def map[A, B](fa: F[A])(f: A => B): F[B] = inst.map(fa)([t[_]] => (ft: Functor[t], ta: t[A]) => ft.map(ta)(f))

  given [T]: Functor[Const[T]] with
    def map[A, B](t: T)(f: A => B): T = t

  inline def derived[F[_]](using gen: K1.Generic[F]): Functor[F] = functorGen

trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

object Applicative:
  inline def apply[F[_]](using ff: Applicative[F]): ff.type = ff

  given Applicative[Id] with
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

    def pure[A](a: A): Id[A] = a

    def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = ff(fa)

  given [X](using M: Monoid[X]): Applicative[Const[X]] with
    def map[A, B](fa: Const[X][A])(f: A => B): Const[X][B] = fa

    def pure[A](a: A): Const[X][A] = M.empty

    def ap[A, B](ff: Const[X][A => B])(fa: Const[X][A]): Const[X][B] = M.combine(ff, fa)

  given Applicative[List] with
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    def pure[A](a: A): List[A] = List(a)

    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      for
        f <- ff
        a <- fa
      yield f(a)

  given Applicative[Option] with
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    def pure[A](a: A): Option[A] = Option(a)

    def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      for
        f <- ff
        a <- fa
      yield f(a)

  given Applicative[[A] =>> () => A] with
    def map[A, B](fa: () => A)(f: A => B): () => B = () => f(fa())
    def pure[A](a: A): () => A = () => a
    def ap[A, B](ff: () => A => B)(fa: () => A): () => B = () => ff()(fa())

trait Traverse[F[_]] extends Functor[F]:
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

object Traverse:
  inline def apply[F[_]](using ff: Traverse[F]): ff.type = ff

  given Traverse[Id] with
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

    def traverse[G[_]: Applicative, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] = f(fa)

  given [X]: Traverse[Const[X]] with
    def map[A, B](fa: Const[X][A])(f: A => B): Const[X][B] = fa

    def traverse[G[_], A, B](fa: Const[X][A])(f: A => G[B])(using G: Applicative[G]): G[Const[X][B]] =
      G.pure(fa)

  given traverseGen[F[_]](using inst: K1.Instances[Traverse, F], func: K1.Instances[Functor, F]): Traverse[F] with
    import Functor.functorGen as delegate

    def map[A, B](fa: F[A])(f: A => B): F[B] = delegate[F].map(fa)(f)

    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(using G: Applicative[G]): G[F[B]] =
      inst.traverse[A, G, B](fa)([a, b] => (ga: G[a], f: a => b) => G.map(ga)(f))([a] => (x: a) => G.pure(x))(
        [a, b] => (gf: G[a => b], ga: G[a]) => G.ap(gf)(ga)
      )(
        [t[_]] => (trav: Traverse[t], t0: t[A]) => trav.traverse[G, A, B](t0)(f)
      )

  inline def derived[F[_]](using gen: K1.Generic[F]): Traverse[F] = traverseGen

trait Optional[F[_]]:
  def headOption[A](fa: F[A]): Option[A]

trait NonEmpty[F[_]] extends Optional[F]:
  def head[A](fa: F[A]): A
  def headOption[A](fa: F[A]): Option[A] = Some(head(fa))

object Optional:
  given Optional[List] with
    def headOption[A](fa: List[A]): Option[A] = fa.headOption
  given Optional[Option] with
    def headOption[A](fa: Option[A]): Option[A] = fa
  given [T]: Optional[Const[T]] with
    def headOption[A](fa: T): Option[A] = None
  given cons: NonEmpty[::] with
    def head[A](fa: ::[A]): A = fa.head
  given NonEmpty[Some] with
    def head[A](fa: Some[A]): A = fa.get
  given NonEmpty[Id] with
    def head[A](fa: A): A = fa

object NonEmpty:
  inline given product[F[_]](using gen: K1.ProductGeneric[F]): NonEmpty[F] =
    // Evidence that at least one of the `Optional` instances is `NonEmpty`.
    K1.summonFirst[NonEmpty, gen.MirroredElemTypes]
    new Product[F](summonInline)

  given coproduct[F[_]](using inst: => K1.CoproductInstances[NonEmpty, F]): NonEmpty[F] with
    def head[A](fa: F[A]): A = inst.fold(fa)([f[_]] => (ne: NonEmpty[f], fa: f[A]) => ne.head(fa))

  inline def derived[F[_]](using gen: K1.Generic[F]): NonEmpty[F] =
    inline gen match
      case given K1.ProductGeneric[F] => product[F]
      case given K1.CoproductGeneric[F] => coproduct[F]

  class Product[F[_]](inst: K1.ProductInstances[Optional, F]) extends NonEmpty[F]:
    def head[A](fa: F[A]): A = headOption(fa).get
    override def headOption[A](fa: F[A]): Option[A] =
      inst.foldLeft(fa)(Option.empty[A]) {
        [f[_]] => (acc: Option[A], opt: Optional[f], fa: f[A]) => Complete(acc.isDefined)(acc)(opt.headOption(fa))
      }

end NonEmpty

trait Foldable[F[_]]:
  def foldLeft[A, B](fa: F[A])(b: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A])(lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

object Foldable:

  inline def apply[F[_]](using ff: Foldable[F]): ff.type = ff

  given Foldable[Id] with
    def foldLeft[A, B](fa: Id[A])(b: B)(f: (B, A) => B): B = f(b, fa)

    def foldRight[A, B](fa: Id[A])(lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa, lb)

  given [X]: Foldable[Const[X]] with
    def foldLeft[A, B](fa: Const[X][A])(b: B)(f: (B, A) => B): B = b

    def foldRight[A, B](fa: Const[X][A])(lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb

  given foldableProduct[F[_]](using inst: K1.ProductInstances[Foldable, F]): Foldable[F] with
    def foldLeft[A, B](fa: F[A])(b: B)(f: (B, A) => B): B =
      inst.foldLeft[A, B](fa)(b)(
        [t[_]] => (acc: B, fd: Foldable[t], t0: t[A]) => Continue(fd.foldLeft(t0)(acc)(f))
      )

    def foldRight[A, B](fa: F[A])(lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.foldRight[A, Eval[B]](fa)(lb)(
        [t[_]] => (fd: Foldable[t], t0: t[A], acc: Eval[B]) => Continue(Eval.defer(fd.foldRight(t0)(acc)(f)))
      )

  given foldableCoproduct[F[_]](using inst: K1.CoproductInstances[Foldable, F]): Foldable[F] with
    def foldLeft[A, B](fa: F[A])(b: B)(f: (B, A) => B): B =
      inst.fold[A, B](fa)(
        [t[_]] => (fd: Foldable[t], t0: t[A]) => fd.foldLeft(t0)(b)(f)
      )

    def foldRight[A, B](fa: F[A])(lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold[A, Eval[B]](fa)(
        [t[_]] => (fd: Foldable[t], t0: t[A]) => Eval.defer(fd.foldRight(t0)(lb)(f))
      )

  inline def derived[F[_]](using gen: K1.Generic[F]): Foldable[F] =
    gen.derive(foldableProduct, foldableCoproduct)

trait FunctorK[H[_[_]]]:
  def mapK[A[_], B[_]](af: H[A])(f: A ~> B): H[B]

object FunctorK:
  inline def apply[H[_[_]]](using fh: FunctorK[H]): FunctorK[H] = fh

  given [T]: FunctorK[K11.Id[T]] with
    def mapK[A[_], B[_]](at: A[T])(f: A ~> B): B[T] = f(at)

  given functorKGen[H[_[_]]](using inst: => K11.Instances[FunctorK, H]): FunctorK[H] with
    def mapK[A[_], B[_]](ha: H[A])(f: A ~> B): H[B] =
      inst.map(ha)([t[_[_]]] => (ft: FunctorK[t], ta: t[A]) => ft.mapK(ta)(f))

  given [T]: FunctorK[K11.Const[T]] with
    def mapK[A[_], B[_]](t: T)(f: A ~> B): T = t

  inline def derived[F[_[_]]](using gen: K11.Generic[F]): FunctorK[F] = functorKGen

case class Fix[S[_, _], A](unfix: S[A, Fix[S, A]])

trait Bifunctor[F[_, _]]:
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

object Bifunctor:
  inline def apply[F[_, _]](using bf: Bifunctor[F]): Bifunctor[F] = bf

  def map[S[_, _], A, B](f: A => B)(fsa: Fix[S, A])(using bs: Bifunctor[S]): Fix[S, B] =
    Fix(bs.bimap(fsa.unfix)(f, map(f)))

  given Bifunctor[Tuple2] with
    def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) =
      (f(fab._1), g(fab._2))

  given Bifunctor[Either] with
    def bimap[A, B, C, D](fab: Either[A, B])(f: A => C, g: B => D): Either[C, D] =
      fab match
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))

  given bifunctorGen[F[_, _]](using inst: K2.Instances[Bifunctor, F]): Bifunctor[F] with
    def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
      inst.map(fab)([t[_, _]] => (bft: Bifunctor[t], tab: t[A, B]) => bft.bimap(tab)(f, g))

  given Bifunctor[K2.Id1] with
    def bimap[A, B, C, D](a: A)(f: A => C, g: B => D): C = f(a)

  given Bifunctor[K2.Id2] with
    def bimap[A, B, C, D](b: B)(f: A => C, g: B => D): D = g(b)

  given [T]: Bifunctor[K2.Const[T]] with
    def bimap[A, B, C, D](t: T)(f: A => C, g: B => D): T = t

  inline def derived[F[_, _]](using gen: K2.Generic[F]): Bifunctor[F] = bifunctorGen

trait Case[F, A, B] extends (A => B)

trait Data[F, T, R]:
  def gmapQ(t: T): List[R]

object Data extends Data0:
  def apply[F, T, R](using dt: Data[F, T, R]): Data[F, T, R] = dt

  type DFR[F, R] = [t] =>> Data[F, t, R]

  given dataGen[F, T, R](using inst: K0.ProductInstances[DFR[F, R], T]): Data[F, T, R] =
    mkData[F, T, R](t =>
      inst
        .foldLeft[List[R]](t)(List.empty[R])(
          [t] => (acc: List[R], dt: Data[F, t, R], t: t) => Continue(dt.gmapQ(t) reverse_::: acc)
        )
        .reverse
    )

  given dataGenC[F, T, R](using inst: => K0.CoproductInstances[DFR[F, R], T]): Data[F, T, R] =
    mkData[F, T, R](t =>
      inst.fold[List[R]](t)(
        [t] => (dt: Data[F, t, R], t: t) => dt.gmapQ(t)
      )
    )

trait Data0:
  def mkData[F, T, R](f: T => List[R]): Data[F, T, R] =
    new Data[F, T, R]:
      def gmapQ(t: T): List[R] = f(t)

  inline given [F, T, R]: Data[F, T, R] = summonFrom {
    case fn: Case[F, T, R] => mkData[F, T, R](t => List(fn(t)))
    case _ => mkData[F, T, R](_ => Nil)
  }

trait DataT[F, T]:
  type Out
  def gmapT(t: T): Out

object DataT:
  type Aux[F, T, Out0] = DataT[F, T] { type Out = Out0 }

  def apply[F, T](using dtt: DataT[F, T]): Aux[F, T, dtt.Out] = dtt

  type DF[F] = [t] =>> Aux[F, t, t]

  def mkDataT[F, T, R](f: T => R): Aux[F, T, R] =
    new DataT[F, T]:
      type Out = R
      def gmapT(t: T): R = f(t)

  given dataTGen[F, T](using inst: => K0.Instances[DF[F], T]): Aux[F, T, T] =
    mkDataT[F, T, T](t =>
      inst.map(t)(
        [t] => (dt: Aux[F, t, t], t: t) => dt.gmapT(t)
      )
    )

  inline given [F, T, R]: Aux[F, T, R] = summonFrom {
    case fn: Case[F, T, R] => mkDataT[F, T, R](fn)
    case ev: (T <:< R) => mkDataT[F, T, R](ev)
  }

trait Empty[T]:
  def empty: T

object Empty:
  def apply[T](using et: Empty[T]): Empty[T] = et

  def mkEmpty[T](t: T): Empty[T] =
    new Empty[T]:
      def empty = t

  given Empty[Unit] = mkEmpty(())
  given Empty[Int] = mkEmpty(0)
  given Empty[String] = mkEmpty("")
  given Empty[Boolean] = mkEmpty(false)

  given emptyGen[A](using inst: K0.ProductInstances[Empty, A]): Empty[A] =
    mkEmpty(inst.construct([a] => (A: Empty[a]) => A.empty))

  inline given emptyGenC[A](using gen: K0.CoproductGeneric[A]): Empty[A] =
    mkEmpty(gen.withOnly[Empty, A]([a <: A] => (A: Empty[a]) => A.empty))

  inline def derived[A](using gen: K0.Generic[A]): Empty[A] =
    inline gen match
      case given K0.ProductGeneric[A] => emptyGen
      case given K0.CoproductGeneric[A] => emptyGenC

trait EmptyK[F[_]]:
  def empty[A]: F[A]

object EmptyK:
  def apply[F[_]](using ef: EmptyK[F]): EmptyK[F] = ef

  given emptyK[C](using ec: Empty[C]): EmptyK[Const[C]] with
    def empty[A]: C = ec.empty

  def mkEmptyK[F[_]](f: [a] => () => F[a]): EmptyK[F] =
    new EmptyK[F]:
      def empty[A] = f[A]()

  given emptyKGen[F[_]](using inst: K1.ProductInstances[EmptyK, F]): EmptyK[F] =
    mkEmptyK([t] => () => inst.construct([f[_]] => (F: EmptyK[f]) => F.empty[t]))

  inline given emptyKGenC[F[_]](using gen: K1.CoproductGeneric[F]): EmptyK[F] =
    mkEmptyK[F]([t] => () => gen.withOnly[EmptyK, F[t]]([f[x] <: F[x]] => (F: EmptyK[f]) => F.empty[t]))

  inline def derived[A[_]](using gen: K1.Generic[A]): EmptyK[A] =
    inline gen match
      case given K1.ProductGeneric[A] => emptyKGen
      case given K1.CoproductGeneric[A] => emptyKGenC

trait Alt1[F[_[_]], G[_[_]], T[_]]:
  def fold[A](f: F[T] => A)(g: G[T] => A): A

object Alt1:
  type Of[F[_[_]], G[_[_]]] = [t[_]] =>> Alt1[F, G, t]

  class Alt1F[F[_[_]], G[_[_]], T[_]](ft: F[T]) extends Alt1[F, G, T]:
    def fold[A](f: F[T] => A)(g: G[T] => A): A = f(ft)

  class Alt1G[F[_[_]], G[_[_]], T[_]](gt: G[T]) extends Alt1[F, G, T]:
    def fold[A](f: F[T] => A)(g: G[T] => A): A = g(gt)

  inline given apply[F[_[_]], G[_[_]], T[_]]: Alt1[F, G, T] = summonFrom {
    case ft: F[T] => new Alt1F(ft)
    case gt: G[T] => new Alt1G(gt)
  }

trait Return[F[_]]:
  def pure[A](a: A): F[A]

object Return:
  def apply[F[_]](using F: Return[F]): Return[F] = F

  def from[F[_]](f: [a] => a => F[a]): Return[F] = new Return[F]:
    def pure[A](a: A) = f(a)

  given Return[Id] =
    from([T] => (t: T) => t)

  given pureGen[A[_]](using inst: K1.ProductInstances[Alt1.Of[Return, EmptyK], A]): Return[A] = from[A]:
    [t] => (a: t) => inst.construct([f[_]] => (af: Alt1.Of[Return, EmptyK][f]) => af.fold[f[t]](_.pure(a))(_.empty[t]))

  inline given pureGenC[F[_]](using gen: K1.CoproductGeneric[F]): Return[F] = from[F]:
    [t] => (a: t) => gen.withFirst[Return, F[t]]([f[x] <: F[x]] => (F: Return[f]) => F.pure(a))

  inline def derived[A[_]](using gen: K1.Generic[A]): Return[A] = inline gen match
    case given K1.ProductGeneric[A] => pureGen
    case given K1.CoproductGeneric[A] => pureGenC

trait Show[T]:
  def show(t: T): String

object Show:
  inline def apply[T](using st: Show[T]): Show[T] = st

  def mkShow[T](f: T => String): Show[T] =
    new Show[T]:
      def show(t: T): String = f(t)

  given Show[Int] = (_: Int).toString
  given Show[String] = (s: String) => "\"" + s + "\""
  given Show[Boolean] = (_: Boolean).toString

  given showGen[T](using inst: K0.ProductInstances[Show, T], labelling: Labelling[T]): Show[T] with
    def show(t: T): String =
      if labelling.elemLabels.isEmpty then labelling.label
      else
        labelling.elemLabels.zipWithIndex
          .map((label, i) => s"$label: ${inst.project(t)(i)([t] => (st: Show[t], pt: t) => st.show(pt))}")
          .mkString(s"${labelling.label}(", ", ", ")")

  given showGenC[T](using inst: K0.CoproductInstances[Show, T]): Show[T] with
    def show(t: T): String = inst.fold(t)([t] => (st: Show[t], t: t) => st.show(t))

  inline def derived[A](using gen: K0.Generic[A]): Show[A] =
    gen.derive(showGen, showGenC)

trait Read[T]:
  def read(s: String): Option[(T, String)]

object Read:
  inline def apply[T](using rt: Read[T]): Read[T] = rt

  import scala.util.matching.Regex
  import scala.util.Try

  def head(s: String, r: Regex): Option[(String, String)] =
    val st = s.trim
    if st == null then None
    else
      st match
        case r(hd, tl) => Some((hd, tl))
        case _ => None

  def readPrimitive[T](r: Regex, f: String => Option[T]): Read[T] =
    (s: String) =>
      for
        (hd, tl) <- head(s, r)
        p <- f(hd)
      yield (p, tl)

  given Read[Int] = readPrimitive("""(-?\d*)(.*)""".r, s => Try(s.toInt).toOption)
  given Read[String] = (s: String) => head(s, """"(.*)"(.*)""".r)
  given Read[Boolean] = readPrimitive("""(true|false)(.*)""".r, s => Try(s.toBoolean).toOption)

  given readGen[T](using inst: K0.ProductInstances[Read, T], labelling: Labelling[T]): Read[T] with
    def read(s: String): Option[(T, String)] =
      def readUnit(s: String): Option[(T, String)] =
        inst
          .unfold[Unit](())(
            [t] => (u: Unit, rt: Read[t]) => ((), None)
          )
          ._2
          .map(t => (t, s))

      def readElems(s: String): Option[(T, String)] =
        type Acc = (String, Seq[String], Boolean)
        inst.unfold[Acc]((s, labelling.elemLabels, true))(
          [t] =>
            (acc: Acc, rt: Read[t]) =>
              val (s, labels, first) = acc
              (for
                (_, tl0) <- if first then Some(("", s)) else head(s, "(,)(.*)".r)
                (_, tl1) <- head(tl0, s"(${labels.head}):(.*)".r)
                (t, tl2) <- rt.read(tl1)
              yield (t, tl2)) match
                case Some(t, tl2) => ((tl2, labels.tail, false), Some(t))
                case None => ((s, labels, first), None)
        ) match
          case (s, None) => None
          case (acc, Some(t)) => Some((t, acc._1))

      if labelling.elemLabels.isEmpty then
        for
          (_, tl0) <- head(s, s"(${labelling.label})(.*)".r)
          (t, tl1) <- readUnit(tl0)
        yield (t, tl1)
      else
        for
          (_, tl0) <- head(s, s"(${labelling.label})\\((.*)".r)
          (t, tl1) <- readElems(tl0)
          (_, tl2) <- head(tl1, s"(\\))(.*)".r)
        yield (t, tl2)

  given readGenC[T](using inst: K0.CoproductInstances[Read, T], labelling: Labelling[T]): Read[T] with
    def read(s: String): Option[(T, String)] =
      labelling.elemLabels.zipWithIndex.iterator.map { (p: (String, Int)) =>
        val (label, i) = p
        if s.trim.nn.startsWith(label) then
          inst.inject[Option[(T, String)]](i)(
            [t <: T] => (rt: Read[t]) => rt.read(s)
          )
        else None
      }.find(_.isDefined).flatten

  inline def derived[A](using gen: K0.Generic[A]): Read[A] =
    gen.derive(readGen, readGenC)

trait Transform[T, U]:
  def apply(t: T): U

object Transform:
  def apply[T, U](using ttu: Transform[T, U]): Transform[T, U] = ttu

  inline def mkField[K, KT, RT <: NonEmptyTuple, V](rt: RT): Object =
    (inline constValue[K0.IndexOf[K, KT]] match
      case -1 => summonInline[Monoid[V]].empty
      case i => rt(i)
    ).asInstanceOf

  inline def mkFieldArray[KU, RU, KT, RT <: NonEmptyTuple](rt: RT): Array[Object] =
    inline erasedValue[(KU, RU)] match
      case _: (Unit, Unit) => Array()
      case _: (Tuple1[k0], Tuple1[v0]) =>
        Array(
          mkField[k0, KT, RT, v0](rt)
        )
      case _: ((k0, k1), (v0, v1)) =>
        Array(
          mkField[k0, KT, RT, v0](rt),
          mkField[k1, KT, RT, v1](rt)
        )
      case _: ((k0, k1, k2), (v0, v1, v2)) =>
        Array(
          mkField[k0, KT, RT, v0](rt),
          mkField[k1, KT, RT, v1](rt),
          mkField[k2, KT, RT, v2](rt)
        )

      // Add fallback for larger sizes

  inline def mkRecord[KU, RU <: Tuple, KT, RT <: NonEmptyTuple](rt: RT): RU =
    Tuple.fromArray(mkFieldArray[KU, RU, KT, RT](rt)).asInstanceOf

  inline given [T, U](using
      gent: K0.ProductGeneric[T] { type MirroredElemTypes <: NonEmptyTuple },
      genu: K0.ProductGeneric[U] { type MirroredElemTypes <: Tuple }
  ): Transform[T, U] = new Transform[T, U]:
    def apply(t: T): U =
      genu.fromRepr(
        mkRecord[genu.MirroredElemLabels, genu.MirroredElemTypes, gent.MirroredElemLabels, gent.MirroredElemTypes](
          gent.toRepr(t)
        )
      )

trait Parser[T]:
  protected def parse(text: String, accum: Boolean): Either[String, T]
  final def parseShort(text: String): Either[String, T] = parse(text, false)
  final def parseAccum(text: String): Either[String, T] = parse(text, true)

object Parser:
  def apply[A: Parser]: Parser[A] = summon

  private val pure = [A] => (a: A) => Right(a)
  private val map = [A, B] => (fa: Either[String, A], f: A => B) => fa.map(f)
  private val ap = [A, B] =>
    (ff: Either[String, A => B], fa: Either[String, A]) =>
      (ff, fa) match
        case (Left(e1), Left(e2)) => Left(e1 + e2)
        case (Left(err), _) => Left(err)
        case (_, Left(err)) => Left(err)
        case (Right(f), Right(a)) => Right(f(a))

  private val tailRecM = [A, B] =>
    (a: A, f: A => Either[String, Either[A, B]]) =>
      @tailrec def loop(a: A): Either[String, B] = f(a) match
        case Left(err) => Left(err)
        case Right(Left(a)) => loop(a)
        case Right(Right(b)) => Right(b)
      loop(a)

  given Parser[String] = (text, _) => Right(text)
  given Parser[Boolean] = (text, _) => text.toBooleanOption.toRight(s"Invalid Boolean '$text';")
  given Parser[Int] = (text, _) => text.toIntOption.toRight(s"Invalid Int '$text';")

  given [A](using inst: K0.ProductInstances[Parser, A], labelling: Labelling[A]): Parser[A] = (text, accum) =>
    val (errors, fields) = text
      .split("\\s*,\\s*")
      .partitionMap(_.split("\\s*=\\s*") match
        case Array(name, value) => Right(name -> value)
        case invalid => Left(s"Invalid field '${invalid.mkString}';")
      )

    if errors.nonEmpty then
      if accum then Left(errors.mkString)
      else Left(errors.head)
    else
      val fieldMap = fields.toMap
      val labels = labelling.elemLabels.iterator
      val parseField = [t] =>
        (parser: Parser[t]) =>
          for
            field <- Right(labels.next())
            value <- fieldMap.get(field).toRight(s"Missing field '$field';")
            parsed <- parser.parse(value, accum)
          yield parsed
      if accum then inst.constructA(parseField)(pure, map, ap)
      else inst.constructM(parseField)(pure, map, tailRecM)

end Parser
