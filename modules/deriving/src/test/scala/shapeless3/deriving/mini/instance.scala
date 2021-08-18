package shapeless3.deriving.mini

import shapeless3.deriving.Functor

class Scope extends IListDefn {

  val F: Functor[IList] = Functor.derived[IList]

}
