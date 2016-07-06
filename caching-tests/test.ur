table tab : {Id : int, Val : int}

fun cache id key nonkey =
    vals <- queryX1 (SELECT T.Val FROM tab AS T WHERE T.Id = {[id]})
                    (fn row => <xml>
                      Val = {[row.Val]}, key = {[key]}<br/>
                    </xml>);
    return <xml><body>
      cache Id = {[id]}, nonkey = {[nonkey]}<br/>
      {vals}
    </body></xml>

fun flush id =
    dml (UPDATE tab SET Val = Val + 1 WHERE Id = {[id]});
    return <xml><body>
      flush Id = {[id]}
    </body></xml>
