package shapeless3.deriving.internals

import scala.compiletime.*
import scala.util.NotGiven

object Kinds:
  /** Like [[Tuple.Head]] but without a bounds restriction. */
  type Head[T] = T match
    case h *: _ => h

  /** Like [[Tuple.Tail]] but without a bounds restriction. */
  type Tail[T] <: Tuple = T match
    case _ *: t => t

  /** For a tuple type `T`, summons the first given element type. */
  transparent inline def summonFirst[T <: Tuple]: Any =
    inline erasedValue[T] match
      case _: (a *: b) =>
        summonFrom:
          case instance: a => instance
          case _ => summonFirst[b]

  /** For a tuple type `T`, summons exactly one given element type. Otherwise fails to compile. */
  transparent inline def summonOnly[T <: Tuple]: Any =
    inline erasedValue[T] match
      case _: (a *: b) =>
        summonFrom:
          case instance: a =>
            summonNone[b]
            instance
          case _ =>
            summonOnly[b]

  /** For a tuple type `T`, proves that none of its element types are given in this scope. */
  transparent inline def summonNone[T <: Tuple]: Unit =
    inline erasedValue[T] match
      case _: EmptyTuple => ()
      case _: (a *: b) =>
        summonInline[NotGiven[a]]
        summonNone[b]
