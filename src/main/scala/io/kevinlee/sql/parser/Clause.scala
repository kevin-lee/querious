package io.kevinlee.sql.parser

/**
  * @author Kevin Lee
  * @since 2017-07-15
  */
trait Clause {
  def toQl: String
  override def toString: String = toQl
}

case class Where(clauses: Seq[Clause])

case class And(left: Clause, right: Clause) extends Clause {
  val toQl: String = s"($left AND $right)"
}
case class Or(left: Clause, right: Clause) extends Clause {
  val toQl: String = s"($left OR $right)"
}


trait Predicate[T] {
  def field: String
  def value: T
}

trait PredicateClause[T] extends Clause {
  def apply(other: T): Boolean
}

object BooleanPredicates {
  sealed trait BooleanPredicate extends Predicate[Boolean] with PredicateClause[Boolean]

  case class Eq(field: String, value: Boolean) extends BooleanPredicate {
    def apply(other: Boolean): Boolean = value == other

    val toQl = s"$field = $value"
  }
  case class Ne(field: String, value: Boolean) extends BooleanPredicate {
    def apply(other: Boolean): Boolean = value != other

    val toQl = s"$field != $value"
  }
}

object NumberPredicates {

  sealed trait NumberPredicate extends Predicate[BigDecimal] with PredicateClause[BigDecimal]

  case class Eq(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(fieldValue: BigDecimal): Boolean = fieldValue == value

    val toQl = s"$field = $value"
  }
  case class Ne(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(fieldValue: BigDecimal): Boolean = fieldValue != value

    val toQl = s"$field != $value"
  }
  case class Lt(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(fieldValue: BigDecimal): Boolean = fieldValue <  value

    val toQl = s"$field < $value"
  }
  case class Le(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(fieldValue: BigDecimal): Boolean = fieldValue <= value

    val toQl = s"$field <= $value"
  }
  case class Gt(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(fieldValue: BigDecimal): Boolean = fieldValue > value

    val toQl = s"$field > $value"
  }
  case class Ge(field: String, value: BigDecimal) extends NumberPredicate {
    def apply(fieldValue: BigDecimal): Boolean = fieldValue >= value

    val toQl = s"$field >= $value"
  }
}

object StringPredicates {

  sealed trait StringPredicate extends Predicate[String] with PredicateClause[String]

  case class Eq(field: String, value: String) extends StringPredicate {
    def apply(fieldValue: String): Boolean = fieldValue == value

    val toQl = s"$field = '$value'"
  }
  case class Ne(field: String, value: String) extends StringPredicate {
    def apply(fieldValue: String): Boolean = fieldValue != value

    val toQl = s"$field != '$value'"
  }
  case class Lt(field: String, value: String) extends StringPredicate {
    def apply(fieldValue: String): Boolean = fieldValue < value

    val toQl = s"$field < '$value'"
  }
  case class Le(field: String, value: String) extends StringPredicate {
    def apply(fieldValue: String): Boolean = fieldValue <= value

    val toQl = s"$field <= '$value'"
  }
  case class Gt(field: String, value: String) extends StringPredicate {
    def apply(fieldValue: String): Boolean = fieldValue > value

    val toQl = s"$field > '$value'"
  }
  case class Ge(field: String, value: String) extends StringPredicate {
    def apply(fieldValue: String): Boolean = fieldValue >= value

    val toQl = s"$field >= '$value'"
  }
}




// Boolean: =, !=
// Number: =, !=, <, <=, >, >=
// String: =, !=, <, <=, >, >=

// WHERE a = 10 AND b < 10


