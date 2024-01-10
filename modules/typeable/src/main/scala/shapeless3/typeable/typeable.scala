/*
 * Copyright (c) 2011-19 Miles Sabin
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

package shapeless3.typeable

import scala.quoted.*

/**
 * Type class supporting type safe cast.
 *
 * @author
 *   Miles Sabin
 */
trait Typeable[T] extends Serializable:
  def cast(t: Any): Option[T] = if castable(t) then Some(t.asInstanceOf[T]) else None
  def castable(t: Any): Boolean
  def describe: String
  override def toString = s"Typeable[$describe]"

object syntax:
  object typeable:
    extension [T](t: T)

      /**
       * Cast the receiver to a value of type `U` if possible. This operation will be as precise wrt erasure as possible
       * given the in-scope `Typeable` instances available.
       */
      inline def cast[U](using tu: Typeable[U]): Option[U] = tu.cast(t)

      /**
       * Test whether the receiver can be cast to a value of type `U`. This operation will be as precise wrt erasure as
       * possible given the in-scope `Typeable` instances available.
       */
      inline def castable[U](using tu: Typeable[U]): Boolean = tu.castable(t)

      /**
       * Cast the receiver to a value of subtype `U` of the receiver's static type if possible. This operation will be
       * as precise wrt erasure as possible given the in-scope `Typeable` instances available.
       */
      inline def narrowTo[U](using ev: U <:< T, tu: Typeable[U]): Option[U] = t.cast[U]

/**
 * Provides instances of `Typeable`.
 */
object Typeable extends Typeable0:
  import java.lang as jl
  import scala.reflect.ClassTag
  import syntax.typeable.*

  inline def apply[T](using tt: Typeable[T]): Typeable[T] = tt

  case class ValueTypeable[T, B](cB: Class[B], describe: String) extends Typeable[T]:
    def castable(t: Any): Boolean = t != null && cB.isInstance(t)

    // Avoid referencing `Constable` (the LUB of `Class` and `String` on JDK 12+) which is not supported by Scala.js
    override def productElement(n: Int): Any = n match
      case 0 => cB: Any
      case 1 => describe: Any
      case _ => throw new IndexOutOfBoundsException(n.toString)

  /** Typeable instance for `Byte`. */
  given byteTypeable: Typeable[Byte] = ValueTypeable[Byte, jl.Byte](classOf[jl.Byte], "Byte")

  /** Typeable instance for `Short`. */
  given shortTypeable: Typeable[Short] = ValueTypeable[Short, jl.Short](classOf[jl.Short], "Short")

  /** Typeable instance for `Char`. */
  given charTypeable: Typeable[Char] = ValueTypeable[Char, jl.Character](classOf[jl.Character], "Char")

  /** Typeable instance for `Int`. */
  given intTypeable: Typeable[Int] = ValueTypeable[Int, jl.Integer](classOf[jl.Integer], "Int")

  /** Typeable instance for `Long`. */
  given longTypeable: Typeable[Long] = ValueTypeable[Long, jl.Long](classOf[jl.Long], "Long")

  /** Typeable instance for `Float`. */
  given floatTypeable: Typeable[Float] = ValueTypeable[Float, jl.Float](classOf[jl.Float], "Float")

  /** Typeable instance for `Double`. */
  given doubleTypeable: Typeable[Double] = ValueTypeable[Double, jl.Double](classOf[jl.Double], "Double")

  /** Typeable instance for `Boolean`. */
  given booleanTypeable: Typeable[Boolean] = ValueTypeable[Boolean, jl.Boolean](classOf[jl.Boolean], "Boolean")

  /** Typeable instance for `Unit`. */
  given unitTypeable: Typeable[Unit] =
    ValueTypeable[Unit, scala.runtime.BoxedUnit](classOf[scala.runtime.BoxedUnit], "Unit")

  /** Typeable instance for `java.lang.Byte`. */
  given jlByteTypeable: Typeable[jl.Byte] = ValueTypeable[jl.Byte, jl.Byte](classOf[jl.Byte], "java.lang.Byte")

  /** Typeable instance for `java.lang.Short`. */
  given jlShortTypeable: Typeable[jl.Short] = ValueTypeable[jl.Short, jl.Short](classOf[jl.Short], "java.lang.Short")

  /** Typeable instance for `java.lang.Character`. */
  given jlCharacterTypeable: Typeable[jl.Character] =
    ValueTypeable[jl.Character, jl.Character](classOf[jl.Character], "java.lang.Character")

  /** Typeable instance for `java.lang.Integer`. */
  given jlIntegerTypeable: Typeable[jl.Integer] =
    ValueTypeable[jl.Integer, jl.Integer](classOf[jl.Integer], "java.lang.Integer")

  /** Typeable instance for `java.lang.Long`. */
  given jlLongTypeable: Typeable[jl.Long] = ValueTypeable[jl.Long, jl.Long](classOf[jl.Long], "java.lang.Long")

  /** Typeable instance for `java.lang.Float`. */
  given jlFloatTypeable: Typeable[jl.Float] = ValueTypeable[jl.Float, jl.Float](classOf[jl.Float], "java.lang.Float")

  /** Typeable instance for `java.lang.Double`. */
  given jlDoubleTypeable: Typeable[jl.Double] =
    ValueTypeable[jl.Double, jl.Double](classOf[jl.Double], "java.lang.Double")

  /** Typeable instance for `java.lang.Boolean`. */
  given jlBooleanTypeable: Typeable[jl.Boolean] =
    ValueTypeable[jl.Boolean, jl.Boolean](classOf[jl.Boolean], "java.lang.Boolean")

  /** Typeable instance for `scala.runtime.BoxedUnit`. */
  given srBoxedUnitTypeable: Typeable[scala.runtime.BoxedUnit] =
    ValueTypeable[scala.runtime.BoxedUnit, scala.runtime.BoxedUnit](
      classOf[scala.runtime.BoxedUnit],
      "scala.runtime.BoxedUnit"
    )

  def isAnyValClass[T](clazz: Class[T]): Boolean =
    clazz == classOf[jl.Byte] ||
      clazz == classOf[jl.Short] ||
      clazz == classOf[jl.Integer] ||
      clazz == classOf[jl.Long] ||
      clazz == classOf[jl.Float] ||
      clazz == classOf[jl.Double] ||
      clazz == classOf[jl.Boolean] ||
      clazz == classOf[jl.Character] ||
      clazz == classOf[scala.runtime.BoxedUnit]

  /** Typeable instance for `Any`. */
  given anyTypeable: Typeable[Any] with
    def castable(t: Any): Boolean = true
    def describe = "Any"

  /** Typeable instance for `AnyVal`. */
  given anyValTypeable: Typeable[AnyVal] with
    def castable(t: Any): Boolean = t != null && isAnyValClass(t.getClass)
    def describe = "AnyVal"

  /** Typeable instance for `AnyRef`. */
  given anyRefTypeable: Typeable[AnyRef] with
    def castable(t: Any): Boolean = t != null && !isAnyValClass(t.getClass)
    def describe = "AnyRef"

  /**
   * Typeable instance for `Iterable`. Note that the contents be will tested for conformance to the element type.
   */
  given iterableTypeable[CC[t] <: Iterable[t], T](using CCTag: ClassTag[CC[Any]], tt: Typeable[T]): Typeable[CC[T]] with
    def castable(t: Any): Boolean = t match
      case cc: CC[?] @unchecked if CCTag.runtimeClass.isAssignableFrom(t.getClass) =>
        cc.forall(_.castable[T])
      case _ => false
    def describe = s"${CCTag.runtimeClass.getSimpleName}[${tt.describe}]"

  /**
   * Typeable instance for `Map`. Note that the contents will be tested for conformance to the key/value types.
   */
  given mapTypeable[M[k, v] <: Map[k, v], K, V](using
      MTag: ClassTag[M[Any, Any]],
      tk: Typeable[K],
      tv: Typeable[V]
  ): Typeable[M[K, V]] with
    def castable(t: Any): Boolean = t match
      case m: Map[Any, Any] @unchecked if MTag.runtimeClass.isAssignableFrom(t.getClass) =>
        m.forall { case (k, v) => k.castable[K] && v.castable[V] }
      case _ => false
    def describe = s"${MTag.runtimeClass.getSimpleName}[${tk.describe}, ${tv.describe}]"

  /** Typeable instance for simple monomorphic types */
  def namedSimpleTypeable[T](clazz: Class[T], name: String): Typeable[T] =
    new Typeable[T]:
      def castable(t: Any): Boolean = t != null && clazz.isAssignableFrom(t.getClass)
      def describe = name

  /** Typeable instance for singleton value types */
  def valueSingletonTypeable[T](value: T, name: String): Typeable[T] =
    new Typeable[T]:
      def castable(t: Any): Boolean = t == value
      def describe = s"$name($value)"

  /**
   * Typeable instance for singleton reference types
   *
   * @param value
   *   The singleton value
   *
   * @param name
   *   The name of the singleton
   *
   * @param serializable
   *   Whether the instance should be serializable. For singleton types of object definitions and symbols, this should
   *   be true, since they preserve their identity after serialization/deserialization. For other cases, it should be
   *   false, since the deserialized instance would lose its singleton property.
   */
  def referenceSingletonTypeable[T](value: T, name: String, serializable: Boolean): Typeable[T] =
    new Typeable[T]:
      def castable(t: Any): Boolean = t.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]
      def describe = s"$name.type"

      @throws(classOf[java.io.IOException])
      private def writeObject(out: java.io.ObjectOutputStream): Unit =
        if serializable then out.defaultWriteObject()
        else throw new java.io.NotSerializableException("referenceSingletonTypeable")

  /** Typeable instance for intersection types with typeable conjuncts */
  def intersectionTypeable[T](parents: Seq[Typeable[?]]): Typeable[T] =
    new Typeable[T]:
      def castable(t: Any): Boolean = t != null && parents.forall(_.castable(t))
      def describe = parents.map(_.describe).mkString(" & ")

  /** Typeable instance for union types with typeable disjuncts */
  def unionTypeable[T](parents: Seq[Typeable[?]]): Typeable[T] =
    new Typeable[T]:
      def castable(t: Any): Boolean = t != null && parents.exists(_.castable(t))
      def describe = parents.map(_.describe).mkString(" | ")

  /** Typeable instance for polymorphic case classes with typeable elements. */
  def namedCaseClassTypeable[T](clazz: Class[T], fields: Seq[Typeable[?]], name: String): Typeable[T] =
    new Typeable[T]:
      def castable(t: Any): Boolean =
        t match
          case p: Product if clazz.isAssignableFrom(t.getClass) =>
            fields.iterator.zip(p.productIterator).forall { case (f, p) => f.castable(p) }
          case _ => false
      def describe = name

  /** Typeable instance for polymorphic sums with typeable elements. */
  def namedSumTypeable[T](elems: Seq[Typeable[?]], name: String): Typeable[T] =
    new Typeable[T]:
      def castable(t: Any): Boolean = elems.exists(_.castable(t))
      def describe = name

  /** Allows constructing a Typeable instance for recursive types by tying the knot with a lazy val. */
  def recursive[T](f: Typeable[T] => Typeable[T]): Typeable[T] = new Typeable[T]:
    lazy val delegate = f(this)
    export delegate.*

trait Typeable0:
  inline def mkDefaultTypeable[T]: Typeable[T] =
    ${ TypeableMacros.impl[T] }

  inline given [T]: Typeable[T] =
    Typeable.recursive: self =>
      given Typeable[T] = self
      mkDefaultTypeable[T]

object TypeableMacros:
  import Typeable.*

  def impl[T: Type](using Quotes): Expr[Typeable[T]] =
    import quotes.reflect.*

    val TypeableType = TypeRepr.of[Typeable[?]] match
      case tp: AppliedType => tp.tycon

    val target = TypeRepr.of[T]

    def isAbstract(tp: TypeRepr): Boolean =
      tp.typeSymbol.isAbstractType ||
        (tp match
          case tp: AppliedType =>
            isAbstract(tp.tycon) || tp.args.exists(isAbstract)
          case _ => false
        )

    def normalize(tp: TypeRepr): TypeRepr = tp match
      case tp: TypeBounds => tp.low
      case tp => tp

    def simpleName(tp: TypeRepr): String =
      normalize(tp).dealias match
        case tp: AppliedType =>
          simpleName(tp.tycon) + tp.args.map(simpleName).mkString("[", ", ", "]")
        case TypeRef(_, name) => name
        case tp => tp.show

    def collectConjuncts(tp: TypeRepr): List[TypeRepr] = tp match
      case tp: AndType =>
        collectConjuncts(tp.left) ++ collectConjuncts(tp.right)
      case tp => List(tp)

    def collectDisjuncts(tp: TypeRepr): List[TypeRepr] = tp match
      case tp: OrType =>
        collectDisjuncts(tp.left) ++ collectDisjuncts(tp.right)
      case tp => List(tp)

    def summonAllTypeables(tps: Seq[TypeRepr]): Option[Expr[Seq[Typeable[?]]]] =
      val ttps = tps.map(tp => TypeableType.appliedTo(tp))
      val instances = ttps.flatMap(ttp =>
        Implicits.search(ttp) match
          case iss: ImplicitSearchSuccess => List(iss.tree.asExprOf[Typeable[?]])
          case _: ImplicitSearchFailure => Nil
      )

      if tps.length == instances.length then Some(Expr.ofSeq(instances))
      else None

    def mkCaseClassTypeable =
      val sym = target.classSymbol.get
      val fields = sym.declaredFields
      val caseFields = sym.caseFields.filter(f => fields.contains(f))
      def fieldTpe(f: Symbol) = f.tree match
        case tree: ValDef => tree.tpt.tpe
      if !fields.forall(f => caseFields.contains(f) || !isAbstract(fieldTpe(f))) then
        report.errorAndAbort(s"No Typeable for case class ${target.show} with non-case fields")
      else
        summonAllTypeables(caseFields.map(target.memberType)) match
          case None =>
            report.errorAndAbort(s"Missing Typeable for field of case class ${target.show}")
          case Some(ftps) =>
            val clazz = Ref(defn.Predef_classOf).appliedToType(target).asExprOf[Class[T]]
            val name = Expr(simpleName(target))
            '{ namedCaseClassTypeable($clazz, $ftps, $name) }

    def mkSumTypeable =
      val r = new ReflectionUtils(quotes)
      import r.*

      Mirror(target) match
        case Some(rm) =>
          val elemTps = rm.MirroredElemTypes
          summonAllTypeables(elemTps) match
            case None =>
              report.errorAndAbort(s"Missing Typeable for child of sum type ${target.show}")
            case Some(etps) =>
              val name = Expr(simpleName(target))
              '{ namedSumTypeable[T]($etps, $name) }

        case None =>
          report.errorAndAbort(s"Typeable for sum type ${target.show} with no Mirror")

    def mkNamedSimpleTypeable =
      val name = Expr(simpleName(target))
      val clazz = Ref(defn.Predef_classOf).appliedToType(target).asExprOf[Class[T]]
      '{ namedSimpleTypeable($clazz, $name) }

    target.dealias match
      case tp: TermRef =>
        val ident = Ident(tp).asExprOf[T]
        val sym = tp.termSymbol
        val name = Expr(sym.name.toString)
        val serializable = Expr(sym.flags.is(Flags.Module))
        '{ referenceSingletonTypeable[T]($ident, $name, $serializable) }

      case ConstantType(c) =>
        val value = Literal(c).asExprOf[T]
        val name = Expr(target.widen.typeSymbol.name.toString)
        '{ valueSingletonTypeable[T]($value, $name) }

      case tp: TypeRef =>
        val qual = tp.qualifier match
          case NoPrefix() => None
          case tp: ThisType => Some(tp.tref)
          case tp => Some(tp)

        val sym = tp.typeSymbol

        def normalizeModuleClass(sym: Symbol): Symbol =
          if sym.flags.is(Flags.Module) then sym.companionModule else sym

        val owner = normalizeModuleClass(sym.owner)

        qual match
          case Some(_) if sym.flags.is(Flags.Case) => mkCaseClassTypeable
          case None => mkNamedSimpleTypeable
          case Some(tp: TypeRef) if normalizeModuleClass(tp.typeSymbol) == owner => mkNamedSimpleTypeable
          case Some(tp: TermRef) if normalizeModuleClass(tp.termSymbol) == owner => mkNamedSimpleTypeable
          case Some(_) if sym.flags.is(Flags.Sealed) => mkSumTypeable
          case _ => report.errorAndAbort(s"No Typeable for type ${target.show} with a dependent prefix")

      case tp: AppliedType =>
        if tp.typeSymbol.flags.is(Flags.Case) then mkCaseClassTypeable
        else if tp.typeSymbol.flags.is(Flags.Sealed) then mkSumTypeable
        else report.errorAndAbort(s"No Typeable for parametrized type ${target.show}")

      case tp: AndType =>
        summonAllTypeables(collectConjuncts(tp)) match
          case Some(ctps) =>
            '{ intersectionTypeable($ctps) }
          case None =>
            report.errorAndAbort(s"No Typeable for & type ${target.show} with missing conjunct(s)")

      case tp: OrType =>
        val disjuncts = collectDisjuncts(tp)
        summonAllTypeables(disjuncts) match
          case Some(dtps) =>
            '{ unionTypeable($dtps) }
          case None =>
            report.errorAndAbort(s"No Typeable for | type ${target.show} with missing disjunct(s)")

      case _ =>
        report.errorAndAbort(s"No Typeable for type ${target.show}")

/**
 * Extractor for use of `Typeable` in pattern matching.
 *
 * Thanks to Stacy Curl for the idea.
 *
 * @author
 *   Miles Sabin
 */
trait TypeCase[T] extends Serializable:
  def unapply(t: Any): Option[T]

object TypeCase:
  import syntax.typeable.*
  def apply[T](using tt: Typeable[T]): TypeCase[T] =
    new TypeCase[T]:
      def unapply(t: Any): Option[T] = t.cast[T]
      override def toString = s"TypeCase[${tt.describe}]"
