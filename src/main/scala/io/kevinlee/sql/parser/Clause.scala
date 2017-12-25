package io.kevinlee.sql.parser

/**
  * @author Kevin Lee
  * @since 2017-07-15
  */
trait Clause {
}

case class Where(clauses: Seq[Clause])

case class And(left: Clause, right: Clause) extends Clause
case class Or(left: Clause, right: Clause) extends Clause

trait Predicate[T] {
  def field: String
  def value: T
}

trait PredicateClause[T] extends Clause {
  def apply(other: T): Boolean
}

object Bools {
  sealed trait BoolPredicate extends Predicate[Boolean] with PredicateClause[Boolean]

  case class Eq(field: String, value: Boolean) extends BoolPredicate {
    def apply(other: Boolean): Boolean = value == other
  }
  case class Ne(field: String, value: Boolean) extends BoolPredicate {
    def apply(other: Boolean): Boolean = value != other
  }
}

object Numbers {

trait NumberClause
  sealed trait NumberPredicate extends Predicate[BigDecimal] with PredicateClause[BigDecimal]

  case class Eq(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(other: BigDecimal): Boolean = value == other
  }
  case class Ne(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(other: BigDecimal): Boolean = value != other
  }
  case class Lt(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(other: BigDecimal): Boolean = value < other
  }
  case class Le(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(other: BigDecimal): Boolean = value <= other
  }
  case class Gt(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(other: BigDecimal): Boolean = value > other
  }
  case class Ge(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(other: BigDecimal): Boolean = value >= other
  }
}

object Strings {
  sealed trait StringPredicate extends Predicate[String] with PredicateClause[String]
  case class Eq(field: String, value: String) extends StringPredicate {
    def apply(other: String): Boolean = value == other
  }
  case class Ne(field: String, value: String) extends StringPredicate {
    def apply(other: String): Boolean = value != other
  }
  case class Lt(field: String, value: String) extends StringPredicate {
    def apply(other: String): Boolean = value < other
  }
  case class Le(field: String, value: String) extends StringPredicate {
    def apply(other: String): Boolean = value <= other
  }
  case class Gt(field: String, value: String) extends StringPredicate {
    def apply(other: String): Boolean = value > other
  }
  case class Ge(field: String, value: String) extends StringPredicate {
    def apply(other: String): Boolean = value >= other
  }
}




// Boolean: =, !=
// Number: =, !=, <, <=, >, >=
// String: =, !=, <, <=, >, >=

// WHERE a = 10 AND b < 10


