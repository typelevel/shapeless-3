/*
 * Copyright (c) 2015-19 Alexandre Archambault
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

import scala.deriving.*
import scala.quoted.*
import shapeless3.deriving.internals.*

import scala.annotation.tailrec

/**
 * Evidence that type `T` has annotation `A`, and provides an instance of the annotation.
 *
 * If type `T` has an annotation of type `A`, then an implicit `Annotation[A, T]` can be found, and its `apply` method
 * provides an instance of the annotation.
 *
 * Example:
 * {{{
 *   case class First(i: Int)
 *
 *   @First(3) trait Something
 *
 *
 *   val somethingFirst = Annotation[First, Something].apply()
 *   assert(somethingFirst == First(3))
 * }}}
 *
 * @tparam A:
 *   annotation type
 * @tparam T:
 *   annotated type
 *
 * @author
 *   Alexandre Archambault
 */
trait Annotation[A, T] extends Serializable:
  def apply(): A

object Annotation:
  def apply[A, T](implicit annotation: Annotation[A, T]): Annotation[A, T] = annotation

  def mkAnnotation[A, T](annotation: A): Annotation[A, T] =
    new Annotation[A, T]:
      def apply() = annotation

  inline def mkAnnotation[A, T]: Annotation[A, T] = ${ AnnotationMacros.mkAnnotation }

  inline given [A, T]: Annotation[A, T] = mkAnnotation[A, T]

/**
 * Provides the annotations of type `A` of the fields of product type or constructors of sum type `T`.
 *
 * If type `T` is a product type, this type class inspects its fields and provides their annotations of type `A`. If
 * type `T` is a sum type, its constructor types are inspected for annotations.
 *
 * Type `Out` is a tuple having the same number of elements as `T` (number of parameters of `T` if `T` is a product
 * type, or number of constructors of `T` if it is a sum type). It is made of `None.type` (no annotation on
 * corresponding field or constructor) and `Some[A]` (corresponding field or constructor is annotated).
 *
 * Method `apply` provides a tuple of type `Out` made of `None` (corresponding field or constructor not annotated) or
 * `Some(annotation)` (corresponding field or constructor has annotation `annotation`).
 *
 * Note that annotation types must be concrete for this type class to take them into account.
 *
 * Example:
 * {{{
 *   case class First(s: String)
 *
 *   case class CC(i: Int, @First("a") s: String)
 *
 *   sealed trait Base
 *   @First("b") case class BaseI(i: Int) extends Base
 *   case class BaseS(s: String) extends Base
 *
 *
 *   val ccFirsts = Annotations[First, CC]
 *   val baseFirsts = Annotations[First, Base]
 *
 *   // ccFirsts.Out is  (None.type, Some[First], None.type)
 *   // ccFirsts.apply() is
 *   //   (None, Some(First("a")), None)
 *
 *   // baseFirsts.Out is  (Some[First], None.type)
 *   // baseFirsts.apply() is
 *   //   (Some(First("b")), None)
 * }}}
 *
 * @tparam A:
 *   annotation type
 * @tparam T:
 *   product or sum type, whose constructor parameters or constructors are annotated
 *
 * @author
 *   Alexandre Archambault
 */
trait Annotations[A, T] extends Serializable:
  type Out <: Tuple

  def apply(): Out

object Annotations:
  def apply[A, T](implicit annotations: Annotations[A, T]): Aux[A, T, annotations.Out] = annotations

  type Aux[A, T, Out0 <: Tuple] = Annotations[A, T] { type Out = Out0 }

  def mkAnnotations[A, T, Out0 <: Tuple](annotations: Out0): Aux[A, T, Out0] =
    new Annotations[A, T]:
      type Out = Out0
      def apply() = annotations

  implicit transparent inline def mkAnnotations[A, T]: Annotations[A, T] =
    ${ AnnotationMacros.mkAnnotations[A, T] }

/**
 * Provides the type annotations of type `A` of the fields of a product type or constructors of a sum type `T`.
 *
 * If type `T` is a product type, this type class inspects its fields and provides their type annotations of type `A`.
 * If type `T` is a sum type, its constructor types are looked for type annotations.
 *
 * Type `Out` is a tuple having the same number of elements as `T` (number of fields of `T` if `T` is a product type, or
 * number of constructors of `T` if it is a sum type). It is made of `None.type` (no annotation on corresponding field
 * or constructor) and `Some[A]` (corresponding field or constructor is annotated).
 *
 * Method `apply` provides a tuple of type `Out` made of `None` (corresponding field or constructor not annotated) or
 * `Some(annotation)` (corresponding field or constructor has annotation `annotation`).
 *
 * Note that type annotations must not be abstract for this type class to take them into account.
 *
 * Example:
 * {{{
 *   case class First(s: String)
 *
 *   case class CC(i: Int, s: String @First("a"))
 *
 *   val ccFirsts = TypeAnnotations[First, CC]
 *
 *   // ccFirsts.Out is  (None.type, Some[First])
 *   // ccFirsts.apply() is (None, Some(First("a")))
 *
 * }}}
 *
 * This implementation is based on [[shapeless.Annotations]] by Alexandre Archambault.
 *
 * @tparam A:
 *   type annotation type
 * @tparam T:
 *   product or sum type, whose fields or constructors are annotated
 *
 * @author
 *   Patrick Grandjean
 */
trait TypeAnnotations[A, T] extends Serializable:
  type Out <: Tuple

  def apply(): Out

object TypeAnnotations:
  def apply[A, T](implicit annotations: TypeAnnotations[A, T]): Aux[A, T, annotations.Out] = annotations

  type Aux[A, T, Out0 <: Tuple] = TypeAnnotations[A, T] { type Out = Out0 }

  def mkAnnotations[A, T, Out0 <: Tuple](annotations: Out0): Aux[A, T, Out0] =
    new TypeAnnotations[A, T]:
      type Out = Out0
      def apply(): Out = annotations

  transparent inline given mkAnnotations[A, T]: TypeAnnotations[A, T] =
    ${ AnnotationMacros.mkTypeAnnotations[A, T] }

/**
 * Provides all variable annotations for the fields of a product type or constructors of a sum type `T`.
 *
 * If type `T` is a product type, this type class inspects its fields and provides their variable annotations. If type
 * `T` is a sum type, its constructor types are looked for variable annotations as well.
 *
 * Type `Out` is a tuple having the same number of elements as `T` (number of fields of `T` if `T` is a product type, or
 * number of constructors of `T` if it is a sum type). It is made of tuples containing all annotations for the
 * corresponding field or constructor (`EmptyTuple` if none).
 *
 * Method `apply` provides a tuple of type `Out` made of tuples containing all annotations of the corresponding field or
 * constructor (`EmptyTuple` if none).
 *
 * Note that variable annotations must not be abstract for this type class to take them into account.
 *
 * Example:
 * {{{
 *   case class First(s: String)
 *   case class Second(i: Int)
 *
 *   case class CC(i: Int, @First("a") @Second(0) s: String)
 *
 *   val ccFirsts = AllAnnotations[CC]
 *
 *   // ccFirsts.Out is  (EmptyTuple, (First, Second))
 *   // ccFirsts.apply() is
 *   //   (EmptyTuple, (First("a"), Second(0)))
 *
 * }}}
 *
 * This implementation is based on [[shapeless.Annotations]] by Alexandre Archambault.
 *
 * @tparam T:
 *   product or sum type, whose fields or constructors are annotated
 *
 * @author
 *   Patrick Grandjean
 */
trait AllAnnotations[T] extends Serializable:
  type Out <: Tuple

  def apply(): Out

object AllAnnotations:
  def apply[T](implicit annotations: AllAnnotations[T]): Aux[T, annotations.Out] = annotations

  type Aux[T, Out0 <: Tuple] = AllAnnotations[T] { type Out = Out0 }

  def mkAnnotations[T, Out0 <: Tuple](annotations: Out0): Aux[T, Out0] =
    new AllAnnotations[T]:
      type Out = Out0
      def apply(): Out = annotations

  transparent inline given mkAnnotations[T]: AllAnnotations[T] =
    ${ AnnotationMacros.mkAllAnnotations[T] }

/**
 * Provides all type annotations for the fields of product type or constructors of sum type `T`.
 *
 * If type `T` is a product type, this type class inspects its fields and provides their type annotations. If type `T`
 * is a sum type, its constructor types are looked for type annotations as well.
 *
 * Type `Out` is a tuple having the same number of elements as `T` (number of fields of `T` if `T` is a product type, or
 * number of constructors of `T` if it is a sum type). It is made of tuples containing all annotations for the
 * corresponding field or constructor (`EmptyTuple` if none).
 *
 * Method `apply` provides a tuple of type `Out` made of tuples containing all annotations for the corresponding field
 * or constructor (`EmptyTuple` if none).
 *
 * Note that type annotations must not be abstract for this type class to take them into account.
 *
 * Example:
 * {{{
 *   case class First(s: String)
 *   case class Second(i: Int)
 *
 *   case class CC(i: Int, s: String @First("a") @Second(0))
 *
 *   val ccFirsts = AllTypeAnnotations[CC]
 *
 *   // ccFirsts.Out is  (EmptyTuple, (First, Second)
 *   // ccFirsts.apply() is
 *   //   (EmptyTuple, (First("a"), Second(0))
 *
 * }}}
 *
 * This implementation is based on [[shapeless.Annotations]] by Alexandre Archambault.
 *
 * @tparam T:
 *   product or sum type, whose fields or constructors are annotated
 *
 * @author
 *   Patrick Grandjean
 */
trait AllTypeAnnotations[T] extends Serializable:
  type Out <: Tuple

  def apply(): Out

object AllTypeAnnotations:
  def apply[T](implicit annotations: AllTypeAnnotations[T]): Aux[T, annotations.Out] = annotations

  type Aux[T, Out0 <: Tuple] = AllTypeAnnotations[T] { type Out = Out0 }

  def mkAnnotations[T, Out0 <: Tuple](annotations: Out0): Aux[T, Out0] =
    new AllTypeAnnotations[T]:
      type Out = Out0
      def apply(): Out = annotations

  transparent inline given mkAnnotations[T]: AllTypeAnnotations[T] =
    ${ AnnotationMacros.mkAllTypeAnnotations[T] }

object AnnotationMacros:
  def mkAnnotation[A: Type, T: Type](using Quotes): Expr[Annotation[A, T]] =
    import quotes.reflect.*

    val annotTpe = TypeRepr.of[A]
    val annotFlags = annotTpe.typeSymbol.flags
    if annotFlags.is(Flags.Abstract) || annotFlags.is(Flags.Trait) then
      report.errorAndAbort(s"Bad annotation type ${annotTpe.show} is abstract")
    else
      val annoteeTpe = TypeRepr.of[T]
      val symbol = if annoteeTpe.isSingleton then annoteeTpe.termSymbol else annoteeTpe.typeSymbol
      symbol.getAnnotation(annotTpe.typeSymbol) match
        case Some(tree) if tree.tpe <:< annotTpe => '{ Annotation.mkAnnotation[A, T](${ tree.asExprOf[A] }) }
        case _ => report.errorAndAbort(s"No Annotation of type ${annotTpe.show} for type ${annoteeTpe.show}")

  def mkAnnotations[A: Type, T: Type](using Quotes): Expr[Annotations[A, T]] =
    mkAnnotationsImpl[A, T, Annotations](ofExprVariableAnnotations)

  def mkTypeAnnotations[A: Type, T: Type](using Quotes): Expr[TypeAnnotations[A, T]] =
    mkAnnotationsImpl[A, T, TypeAnnotations](ofExprTypeAnnotations)

  private def mkAnnotationsImpl[A: Type, T: Type, AS[A, T]: Type](mk: Seq[Expr[Any]] => Expr[AS[A, T]])(using
      Quotes
  ): Expr[AS[A, T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[AS[A, T]] <:< TypeRepr.of[TypeAnnotations[A, T]]
    val annotTpe = TypeRepr.of[A]
    val annotFlags = annotTpe.typeSymbol.flags
    if annotFlags.is(Flags.Abstract) || annotFlags.is(Flags.Trait) then
      report.errorAndAbort(s"Bad annotation type ${annotTpe.show} is abstract")
    else
      val annotations = extractAnnotations[T](tpe)
      val exprs = annotations.map { child =>
        child.find(_.tpe <:< TypeRepr.of[A]) match
          case Some(tree) => '{ Some(${ tree.asExprOf[A] }) }
          case None => '{ None }
      }

      mk(exprs)

  def mkAllAnnotations[T: Type](using Quotes): Expr[AllAnnotations[T]] =
    mkAllAnnotationsImpl[T, AllAnnotations](ofExprAllVariableAnnotations)

  def mkAllTypeAnnotations[T: Type](using Quotes): Expr[AllTypeAnnotations[T]] =
    mkAllAnnotationsImpl[T, AllTypeAnnotations](ofExprAllTypeAnnotations)

  private def mkAllAnnotationsImpl[T: Type, AS[T]: Type](mk: Seq[Expr[Any]] => Expr[AS[T]])(using Quotes): Expr[AS[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[AS[T]] <:< TypeRepr.of[AllTypeAnnotations[T]]
    val annotations = extractAnnotations[T](tpe)
    val exprs = annotations.map { anns =>
      Expr.ofTupleFromSeq(anns.map(_.asExpr))
    }

    mk(exprs)

  def extractAnnotations[T: Type](tpe: Boolean)(using q: Quotes): Seq[List[q.reflect.Term]] =
    val utils = new ReflectionUtils(q)
    import quotes.reflect.*
    import utils.*

    @tailrec def typeAnnotationsOfType(tpe: TypeRepr, acc: List[Term]): List[Term] = tpe match
      case annotated: AnnotatedType => typeAnnotationsOfType(annotated.underlying, annotated.annotation :: acc)
      case alias: TypeRef if alias.typeSymbol.isAliasType => typeAnnotationsOfType(alias.translucentSuperType, acc)
      case _ => acc

    def typeAnnotationsOfTree(tree: Tree, acc: List[Term]): List[Term] = tree match
      case classDef: ClassDef => classDef.parents.flatMap(typeAnnotationsOfTree(_, acc))
      case valDef: ValDef => typeAnnotationsOfTree(valDef.tpt, acc)
      case typeId: TypeIdent => typeAnnotationsOfType(typeId.tpe, acc)
      case inferred: Inferred => typeAnnotationsOfType(inferred.tpe, acc)
      case annotated: Annotated => typeAnnotationsOfTree(annotated.arg, annotated.annotation :: acc)
      case _ => acc

    def annotationsOfSym(sym: Symbol): List[Term] =
      if tpe then typeAnnotationsOfTree(sym.tree, Nil) else sym.annotations.reverse

    def annotationsOfType(tpe: TypeRepr): List[Term] =
      annotationsOfSym(if tpe.isSingleton then tpe.termSymbol else tpe.typeSymbol)

    val annoteeTpe = TypeRepr.of[T]
    annoteeTpe.classSymbol match
      case Some(annoteeCls) if annoteeCls.flags.is(Flags.Case) =>
        annoteeCls.primaryConstructor.paramSymss.find(_.exists(_.isTerm)).getOrElse(Nil).map(annotationsOfSym)
      case Some(_) =>
        Mirror(annoteeTpe) match
          case Some(mirror) => mirror.MirroredElemTypes.map(annotationsOfType)
          case None => report.errorAndAbort(s"No Annotations for type ${annoteeTpe.show} without a Mirror")
      case None =>
        report.errorAndAbort(s"No Annotations for non-class ${annoteeTpe.show}")
  end extractAnnotations

  def ofExprVariableAnnotations[A: Type, T: Type](annotTrees: Seq[Expr[Any]])(using Quotes): Expr[Annotations[A, T]] =
    Expr.ofTupleFromSeq(annotTrees) match
      case '{ $t: tup } => '{ Annotations.mkAnnotations[A, T, tup & Tuple]($t) }

  def ofExprTypeAnnotations[A: Type, T: Type](annotTrees: Seq[Expr[Any]])(using Quotes): Expr[TypeAnnotations[A, T]] =
    Expr.ofTupleFromSeq(annotTrees) match
      case '{ $t: tup } => '{ TypeAnnotations.mkAnnotations[A, T, tup & Tuple]($t) }

  def ofExprAllVariableAnnotations[T: Type](annotTrees: Seq[Expr[Any]])(using Quotes): Expr[AllAnnotations[T]] =
    Expr.ofTupleFromSeq(annotTrees) match
      case '{ $t: tup } => '{ AllAnnotations.mkAnnotations[T, tup & Tuple]($t) }

  def ofExprAllTypeAnnotations[T: Type](annotTrees: Seq[Expr[Any]])(using Quotes): Expr[AllTypeAnnotations[T]] =
    Expr.ofTupleFromSeq(annotTrees) match
      case '{ $t: tup } => '{ AllTypeAnnotations.mkAnnotations[T, tup & Tuple]($t) }
