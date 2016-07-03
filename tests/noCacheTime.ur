table t : { A : int, B : time }

fun main () : transaction page =
    past <- queryX1 (SELECT *
                     FROM t
                     WHERE t.B < CURRENT_TIMESTAMP)
                    (fn r => <xml><li>{[r.B]}: {[r.A]}</li></xml>);
    return <xml><body>
      {past}
    </body></xml>
