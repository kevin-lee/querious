package querious

import StringInterpolation._

/**
 * @author Kevin Lee
 * @since 2021-02-27
 */
object TestData {

  val escapingChars = List("\\\"", "\\/", "\\\\", "\\b", "\\f", "\\n", "\\r", "\\t")
  val escapingCharsToString = escapingChars.map(x => esc""""$x"""").mkString("[", ", ", "]")

  val whereClauseData: List[(String, Clause)] = List(
    "num = 999 OR abc != true OR name = 'Kevin'" ->
      Or(
        NumberPredicates.Eq("num", 999),
        Or(
          BooleanPredicates.Ne("abc", true),
          StringPredicates.Eq("name", "Kevin")
        )
      ),
    "num = 999 AND abc != true AND name = 'Kevin'" ->
      And(
        And(
          NumberPredicates.Eq("num", 999),
          BooleanPredicates.Ne("abc", true)
        ),
        StringPredicates.Eq("name", "Kevin")
      ),
    "num = 999 And abc != true OR name = 'Kevin'" ->
      Or(
        And(
          NumberPredicates.Eq("num", 999),
          BooleanPredicates.Ne("abc", true)
        ),
        StringPredicates.Eq("name", "Kevin")
      ),
    "num = 999 OR abc != true AND name = 'Kevin'" ->
      Or(
        NumberPredicates.Eq("num", 999),
        And(
          BooleanPredicates.Ne("abc", true),
          StringPredicates.Eq("name", "Kevin")
        )
      ),
    "num = 999 OR abc != true OR name = 'Kevin' AND price >= 50.50" ->
      Or(
        NumberPredicates.Eq("num", 999),
        Or(
          BooleanPredicates.Ne("abc", true),
          And(
            StringPredicates.Eq("name", "Kevin"),
            NumberPredicates.Ge("price", 50.50)
          )
        )
      ),
    "item_id =   1   AND   (  price > 100 OR c = true OR name = 'awesome product' AND (something <= 'blah' OR (a = true AND b != true  ) AND d < 12345))" ->
      And(
        NumberPredicates.Eq("item_id", 1),
        Or(
          NumberPredicates.Gt("price", 100),
          Or(
            BooleanPredicates.Eq("c", true),
            And(
              StringPredicates.Eq("name", "awesome product"),
              Or(
                StringPredicates.Le("something", "blah"),
                And(
                  And(
                    BooleanPredicates.Eq("a", true),
                    BooleanPredicates.Ne("b", true)
                  ),
                  NumberPredicates.Lt("d", 12345)
                )
              )
            )
          )
        )
      )
  )
}
