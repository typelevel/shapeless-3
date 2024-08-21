package shapeless3.deriving

import shapeless3.deriving.internals.Kinds

import scala.Tuple.Union

object K21
    extends Kind[
      [_[_], _[_]] =>> Any,
      [_[_], _[_]] =>> Tuple,
      [t[_[_], _[_]]] =>> t[[_] =>> Any, [_] =>> Any],
      [t[_[_], _[_]]] =>> [a[_], b[_]] =>> Kinds.Head[t[a, b]],
      [t[_[_], _[_]]] =>> [a[_], b[_]] =>> Kinds.Tail[t[a, b]]
    ]:

  type Id1[t] = [f[_], g[_]] =>> f[t]
  type Id2[t] = [f[_], g[_]] =>> g[t]
  type Const[c] = [t[_], u[_]] =>> c

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
