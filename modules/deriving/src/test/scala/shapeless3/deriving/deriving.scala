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

import org.junit.Assert.*
import org.junit.Test

import scala.annotation.tailrec
import scala.compiletime.constValueTuple

import cats.Eval

import adts.*
import OptE.{SmE, NnE}

// Tests

object Size:
  given Case[Size.type, Int, Int] = identity(_)
  given Case[Size.type, String, Int] = _.length
  given Case[Size.type, Boolean, Int] = _ => 1

object Inc:
  given Case[Inc.type, Int, Int] = _ + 1
  given Case[Inc.type, String, String] = _ + "!"
  given Case[Inc.type, Boolean, Boolean] = !_

class DerivationTests:
  @Test
  def monoid: Unit =
    val v0 = Monoid[ISB]
    assert(v0.empty == ISB(0, "", false))
    assert(v0.combine(ISB(1, "foo", false), ISB(2, "bar", true)) == ISB(3, "foobar", true))

    val v1 = Monoid[Box[Int]]
    assert(v1.empty == Box(0))
    assert(v1.combine(Box(1), Box(2)) == Box(3))

    val v2 = Monoid[Recursive]
    assert(v2.empty == Recursive(0, None))
    assert(
      v2.combine(Recursive(1, Some(Recursive(2, None))), Recursive(1, Some(Recursive(3, None)))) == Recursive(
        2,
        Some(Recursive(5, None))
      )
    )

  @Test
  def eq: Unit =
    val v0 = Eq[SomeInt]
    assert(v0.eqv(SomeInt(23), SomeInt(23)))
    assert(!v0.eqv(SomeInt(23), SomeInt(13)))
    val v1 = Eq[NoneInt.type]
    assert(v1.eqv(NoneInt, NoneInt))
    val v2 = Eq[OptionInt]
    assert(v2.eqv(SomeInt(23), SomeInt(23)))
    assert(!v2.eqv(SomeInt(23), SomeInt(13)))
    assert(!v2.eqv(SomeInt(23), NoneInt))

    val v3 = Eq[Sm[Int]]
    assert(v3.eqv(Sm(23), Sm(23)))
    assert(!v3.eqv(Sm(23), Sm(13)))
    val v4 = Eq[Nn.type]
    assert(v4.eqv(Nn, Nn))
    val v5 = Eq[Opt[Int]]
    assert(v5.eqv(Sm(23), Sm(23)))
    assert(!v5.eqv(Sm(23), Sm(13)))
    assert(!v5.eqv(Sm(23), Nn))

    val v6 = Eq[CNil.type]
    assert(v6.eqv(CNil, CNil))
    val v7 = Eq[CCons[Int]]
    assert(v7.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
    assert(!v7.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))
    val v8 = Eq[CList[Int]]
    assert(v8.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(2, CCons(3, CNil)))))
    assert(!v8.eqv(CCons(1, CCons(2, CCons(3, CNil))), CCons(1, CCons(4, CCons(3, CNil)))))

    val v9 = Eq[OptE[Int]]
    assert(v9.eqv(SmE(23), SmE(23)))
    assert(!v9.eqv(SmE(23), SmE(13)))
    assert(!v9.eqv(SmE(23), NnE))

  @Test
  def order: Unit =
    val v0 = Ord[Unit]
    assert(v0.compare((), ()) == 0)

    val v1 = Ord[Box[Int]]
    assert(v1.compare(Box(1), Box(1)) == 0)
    assert(v1.compare(Box(1), Box(0)) == 1)
    assert(v1.compare(Box(0), Box(1)) == -1)

    val v2 = Ord[OptionInt]
    assert(v2.compare(NoneInt, NoneInt) == 0)
    assert(v2.compare(SomeInt(0), SomeInt(0)) == 0)
    assert(v2.compare(SomeInt(0), SomeInt(1)) == -1)
    assert(v2.compare(SomeInt(1), SomeInt(0)) == 1)
    assert(v2.compare(SomeInt(0), NoneInt) == 1)

    val v3 = Ord[NoneInt.type]
    assert(v3.compare(NoneInt, NoneInt) == 0)

    val v4 = Ord[SomeInt]
    assert(v4.compare(SomeInt(0), SomeInt(0)) == 0)
    assert(v4.compare(SomeInt(0), SomeInt(1)) == -1)
    assert(v4.compare(SomeInt(1), SomeInt(0)) == 1)

    val v5 = Ord[Opt[String]]
    assert(v5.compare(Nn, Nn) == 0)
    assert(v5.compare(Sm("foo"), Sm("foo")) == 0)
    assert(v5.compare(Sm("foo"), Sm("goo")) == -1)
    assert(v5.compare(Sm("foo"), Sm("eoo")) == 1)
    assert(v5.compare(Sm("foo"), Nn) == 1)

    val v6 = Ord[Nn.type]
    assert(v6.compare(Nn, Nn) == 0)

    val v7 = Ord[Sm[String]]
    assert(v7.compare(Sm("foo"), Sm("foo")) == 0)
    assert(v7.compare(Sm("foo"), Sm("goo")) == -1)
    assert(v7.compare(Sm("foo"), Sm("eoo")) == 1)

    val v8 = Ord[OptE[String]]
    assert(v8.compare(NnE, NnE) == 0)
    assert(v8.compare(SmE("foo"), SmE("foo")) == 0)
    assert(v8.compare(SmE("foo"), SmE("goo")) == -1)
    assert(v8.compare(SmE("foo"), SmE("eoo")) == 1)
    assert(v8.compare(SmE("foo"), NnE) == 1)

  @Test
  def functor: Unit =
    val v0 = Functor[Box]
    assert(v0.map(Box("foo"))(_.length) == Box(3))

    val v1 = Functor[Sm]
    assert(v1.map(Sm("foo"))(_.length) == Sm(3))
    val v2 = Functor[Const[Nn.type]]
    assert(v2.map(Nn)(identity) == Nn)
    val v3 = Functor[Opt]
    assert(v3.map(Sm("foo"))(_.length) == Sm(3))
    assert(v3.map(Nn)(identity) == Nn)

    val v4 = Functor[Const[CNil.type]]
    assert(v4.map(CNil)(identity) == CNil)
    val v5 = Functor[CCons]
    assert(v5.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))
    val v6 = Functor[CList]
    assert(v6.map(CCons("foo", CCons("quux", CCons("wibble", CNil))))(_.length) == CCons(3, CCons(4, CCons(6, CNil))))
    val v7 = Functor[[t] =>> CList[Opt[t]]]
    assert(
      v7.map(CCons(Sm("foo"), CCons(Nn, CCons(Sm("quux"), CNil))))(_.length) == CCons(
        Sm(3),
        CCons(Nn, CCons(Sm(4), CNil))
      )
    )

    val v8 = Functor[OptE]
    assert(v8.map(SmE("foo"))(_.length) == SmE(3))
    assert(v8.map(NnE)(identity) == NnE)

  def mkCList(n: Int) =
    @tailrec
    def loop(n: Int, acc: CList[Int]): CList[Int] =
      if n == 0 then acc
      else loop(n - 1, CCons(1, acc))
    loop(n, CNil)

  @Test
  def foldable: Unit =
    val v0 = Foldable[Box]
    assert(v0.foldLeft(Box(1))(0)((acc: Int, x: Int) => acc + x) == 1)
    assert(v0.foldRight(Box(1))(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 1)

    val v1 = Foldable[Sm]
    assert(v1.foldLeft(Sm(1))(0)((acc: Int, x: Int) => acc + x) == 1)
    assert(v1.foldRight(Sm(1))(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 1)
    val v2 = Foldable[Const[Nn.type]]
    assert(v2.foldLeft(Nn)(0)((acc: Int, x: Int) => acc + x) == 0)
    assert(v2.foldRight(Nn)(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 0)
    val v3 = Foldable[Opt]
    assert(v3.foldLeft(Sm(1))(0)((acc: Int, x: Int) => acc + x) == 1)
    assert(v3.foldRight(Sm(1))(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 1)
    assert(v3.foldLeft(Nn)(0)((acc: Int, x: Int) => acc + x) == 0)
    assert(v3.foldRight(Nn)(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 0)

    val v4 = Foldable[Const[CNil.type]]
    assert(v4.foldLeft(CNil)(0)((acc: Int, x: Int) => acc + x) == 0)
    assert(v4.foldRight(CNil)(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 0)
    val v5 = Foldable[CCons]
    assert(v5.foldLeft(CCons(1, CCons(2, CNil)))(0)((acc: Int, x: Int) => acc + x) == 3)
    assert(v5.foldRight(CCons(1, CCons(2, CNil)))(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 3)
    val v6 = Foldable[CList]
    assert(v6.foldLeft(CNil)(0)((acc: Int, x: Int) => acc + x) == 0)
    assert(v6.foldRight(CNil)(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 0)
    assert(v6.foldLeft(CCons(1, CCons(2, CNil)))(0)((acc: Int, x: Int) => acc + x) == 3)
    assert(v6.foldRight(CCons(1, CCons(2, CNil)))(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 3)
    assert(v6.foldRight(mkCList(20000))(Eval.later(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 20000)

    val v7 = Foldable[OptE]
    assert(v7.foldLeft(SmE(1))(0)((acc: Int, x: Int) => acc + x) == 1)
    assert(v7.foldRight(SmE(1))(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 1)
    assert(v7.foldLeft(NnE)(0)((acc: Int, x: Int) => acc + x) == 0)
    assert(v7.foldRight(NnE)(Eval.now(0))((x: Int, acc: Eval[Int]) => acc.map(_ + x)).value == 0)

  @Test
  def traverse: Unit =
    val v0 = Traverse[Box]
    assert(v0.traverse(Box(1))((x: Int) => List(x + 1)) == List(Box(2)))

    val v1 = Traverse[Sm]
    assert(v1.traverse(Sm(1))((x: Int) => List(x + 1)) == List(Sm(2)))
    val v2 = Traverse[Const[Nn.type]]
    assert(v2.traverse(Nn)((x: Int) => List(x + 1)) == List(Nn))
    val v3 = Traverse[Opt]
    assert(v3.traverse(Sm(1))((x: Int) => List(x + 1)) == List(Sm(2)))
    assert(v3.traverse(Nn)((x: Int) => List(x + 1)) == List(Nn))

    val v4 = Traverse[Const[CNil.type]]
    assert(v4.traverse(CNil)(Option.apply) == Some(CNil))
    val v5 = Traverse[CCons]
    assert(v5.traverse(CList("foo", "bar"))(Option.apply) == Some(CList("foo", "bar")))
    val v6 = Traverse[CList]
    assert(v6.traverse(CList("foo", "bar"))(Option.apply) == Some(CList("foo", "bar")))
    assert(v6.traverse(CNil)(Option.apply) == Some(CNil))
    assert(v6.traverse(CList("foo", "bar"))(() => _).apply() == CList("foo", "bar"))
    assert(v6.traverse(CList(1, 2))(x => List(x, x + 1)) == List(CList(1, 2), CList(1, 3), CList(2, 2), CList(2, 3)))

    val v7 = Traverse[OptE]
    assert(v7.traverse(SmE(1))((x: Int) => List(x + 1)) == List(SmE(2)))
    assert(v7.traverse(NnE)((x: Int) => List(x + 1)) == List(NnE))

    val v8 = Traverse[Phantom]
    assert(v8.traverse(Phantom())(Option.apply) == Some(Phantom()))

  @Test
  def functork: Unit =
    val v0 = FunctorK[Order]
    assert(v0.mapK(Order[OptionD](Given("Epoisse"), Default(10)))(OptionD.fold) == Order[Id]("Epoisse", 10))

  @Test
  def bifunctor: Unit =
    val v0 = Bifunctor[ConsF]
    val v1 = Bifunctor[ListF]
    val v2: ListF.List[String] = Fix(ConsF("foo", Fix(ConsF("quux", Fix(ConsF("wibble", Fix(NilF)))))))
    val v3: ListF.List[Int] = Fix(ConsF(3, Fix(ConsF(4, Fix(ConsF(6, Fix(NilF)))))))
    assert(Bifunctor.map((_: String).length)(v2) == v3)

  @Test
  def data: Unit =
    val v0 = Data[Size.type, ISB, Int]
    assert(v0.gmapQ(ISB(23, "foo", true)).sum == 27)
    val v1 = Data[Size.type, OptionInt, Int]
    assert(v1.gmapQ(NoneInt).sum == 0)
    assert(v1.gmapQ(SomeInt(23)).sum == 23)
    val v2 = Data[Size.type, CList[String], Int]
    assert(v2.gmapQ(CCons("foo", CCons("quux", CCons("wibble", CNil)))).sum == 13)

  @Test
  def datat: Unit =
    val v0 = DataT[Inc.type, ISB]
    assert(v0.gmapT(ISB(23, "foo", true)) == ISB(24, "foo!", false))
    val v1 = DataT[Inc.type, OptionInt]
    assert(v1.gmapT(NoneInt) == NoneInt)
    assert(v1.gmapT(SomeInt(23)) == SomeInt(24))
    val v2 = DataT[Inc.type, CList[Int]]
    assert(v2.gmapT(CCons(1, CCons(2, CCons(3, CNil)))) == CCons(2, CCons(3, CCons(4, CNil))))

  @Test
  def empty: Unit =
    val v0 = Empty[ISB]
    assert(v0.empty == ISB(0, "", false))

  @Test
  def emptyk: Unit =
    val v0 = EmptyK[Opt]
    assert(v0.empty[Int] == Nn)
    val v1 = EmptyK[CList]
    assert(v1.empty[Int] == CNil)

  @Test
  def pure: Unit =
    val v0 = Pure[Box]
    assert(v0.pure(23) == Box(23))
    val v1 = Pure[CList]
    assert(v1.pure(23) == CCons(23, CNil))

  @Test
  def labels: Unit =
    val v0 = K0.Generic[ISB]
    val v1 = constValueTuple[v0.MirroredElemLabels]
    assert(v1 == ("i", "s", "b"))

  @Test
  def show: Unit =
    val v0 = Show[ISB]
    assert(v0.show(ISB(23, "foo", true)) == """ISB(i: 23, s: "foo", b: true)""")

    val v1 = Show[OptionInt]
    assert(v1.show(SomeInt(23)) == "SomeInt(value: 23)")
    assert(v1.show(NoneInt) == "NoneInt")

    val v2 = Show[Box[Int]]
    assert(v2.show(Box(23)) == "Box(x: 23)")

    val v3 = Show[Opt[Int]]
    assert(v3.show(Sm(23)) == "Sm(value: 23)")
    assert(v3.show(Nn) == "Nn")

    val v4 = Show[CList[Int]]
    assert(
      v4.show((CCons(1, CCons(2, CCons(3, CNil))))) == "CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))"
    )

    val v5 = Show[Order[Id]]
    assert(v5.show(Order[Id]("Epoisse", 10)) == """Order(item: "Epoisse", quantity: 10)""")

    val v6 = Show[OptE[Int]]
    assert(v6.show(SmE(23)) == "SmE(value: 23)")
    assert(v6.show(NnE) == "NnE")

  @Test
  def read: Unit =
    val v0 = Read[ISB]
    assert(v0.read("""ISB(i: 23, s: "foo", b: true)""") == Some((ISB(23, "foo", true), "")))

    val v1 = Read[OptionInt]
    assert(v1.read("SomeInt(value: 23)") == Some((SomeInt(23), "")))
    assert(v1.read("NoneInt") == Some((NoneInt, "")))

    val v2 = Read[Box[Int]]
    assert(v2.read("Box(x: 23)") == Some((Box(23), "")))

    val v3 = Read[Opt[Int]]
    assert(v3.read("Sm(value: 23)") == Some((Sm(23), "")))
    assert(v3.read("Nn") == Some((Nn, "")))

    val v4 = Read[CList[Int]]
    assert(
      v4.read("CCons(hd: 1, tl: CCons(hd: 2, tl: CCons(hd: 3, tl: CNil)))") == Some(
        (CCons(1, CCons(2, CCons(3, CNil))), "")
      )
    )

    val v5 = Read[Order[Id]]
    assert(v5.read("""Order(item: "Epoisse", quantity: 10)""") == Some((Order[Id]("Epoisse", 10), "")))

    val v6 = Read[OptE[Int]]
    assert(v6.read("SmE(value: 23)") == Some((SmE(23), "")))
    assert(v6.read("NnE") == Some((NnE, "")))

  @Test
  def transform: Unit =
    val v0 = Transform[BI, ISB]
    assert(v0(BI(true, 23)) == ISB(23, "", true))

  @Test
  def repr: Unit =
    val v0 = K0.ProductGeneric[Box[Int]]
    val v1 = v0.toRepr(Box(23))
    val v1a: Tuple1[Int] = v1
    assert(v1 == Tuple1(23))

    val v2 = K0.ProductGeneric[Order[Id]]
    val v3 = v2.toRepr(Order[Id]("Epoisse", 10))
    val v3a: (String, Int) = v3
    assert(v3 == ("Epoisse", 10))

  @Test
  def parsing: Unit =
    val parser = Parser[ISB]
    // Applicative
    assertEquals(Right(ISB(42, "foo", true)), parser.parseAccum("s=foo,i=42,b=true,hidden=?"))
    assertEquals(Left("Missing field 's';Invalid Boolean 'kinda';"), parser.parseAccum("i=42,b=kinda"))
    assertEquals(Left("Invalid field 'broken';Invalid field '?';"), parser.parseAccum("i=42,broken,?"))
    // Monadic
    assertEquals(Right(ISB(42, "foo", true)), parser.parseShort("s=foo,i=42,b=true,hidden=?"))
    assertEquals(Left("Missing field 's';"), parser.parseShort("i=42,b=kinda"))
    assertEquals(Left("Invalid field 'broken';"), parser.parseShort("i=42,broken,?"))
