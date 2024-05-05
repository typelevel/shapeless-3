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

  transparent inline def summonFirst[T]: Any =
    inline erasedValue[T] match
      case _: (a *: b) =>
        summonFrom {
          case instance: a => instance
          case _ => summonFirst[b]
        }

  transparent inline def summonOnly[T]: Any =
    inline erasedValue[T] match
      case _: (a *: b) =>
        summonFrom {
          case instance: a =>
            summonNone[b]
            instance
          case _ =>
            summonOnly[b]
        }

  transparent inline def summonNone[T]: Unit =
    inline erasedValue[T] match
      case _: EmptyTuple => ()
      case _: (a *: b) =>
        summonInline[NotGiven[a]]
        summonNone[b]
