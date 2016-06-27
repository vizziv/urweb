table tab : {W : int, X : int, Y : int, Z : int, Val : int}

fun cache w x y z =
    res <- oneOrNoRows1 (SELECT T.Val FROM tab AS T
                                      WHERE T.W = {[w]}
                                        AND T.X = {[x]}
                                        AND T.Y = {[y]}
                                        AND T.Z = {[z]});
    return <xml><body>
      cache w={[w]} x={[x]} y={[y]} z={[z]}
      {case res of
           None => <xml>?</xml>
         | Some row => <xml>{[row.Val]}</xml>}
    </body></xml>

fun flush w x y z =
    dml (UPDATE tab
         SET Val = Val * (Val + 2) / Val - 3
         WHERE W = {[w]}
           AND Y = {[y]}
           AND Z = {[z]});
    return <xml><body>
      flush w={[w]} x={[x]} y={[y]} z={[z]}
    </body></xml>

fun flush2 w x y z =
    dml (UPDATE tab
         SET Val = Val * (Val + 2) / Val - 3
         WHERE W = {[w]}
           AND X = {[x]}
           AND Y = {[y]});
    return <xml><body>
      flush' w={[w]} x={[x]} y={[y]} z={[z]}
    </body></xml>


(* fun cacheAlt id = *)
(*     res <- oneOrNoRows (SELECT Q.Id *)
(*                         FROM (SELECT Tab.Id AS Id FROM tab WHERE Tab.Id = {[id]}) *)
(*                         AS Q); *)
(*     return <xml><body> *)
(*       cacheAlt *)
(*       {case res of *)
(*            None => <xml>?</xml> *)
(*          | Some row => <xml>{[row.Q.Id]}</xml>} *)
(*     </body></xml> *)

(* fun sillyRecursive {Id = id : int, FooBar = fooBar} = *)
(*     if fooBar <= 0 *)
(*     then 0 *)
(*     else 1 + sillyRecursive {Id = id, FooBar = fooBar - 1} *)

(* fun cacheR (r : {Id : int, FooBar : int}) = *)
(*     res <- oneOrNoRows (SELECT tab.Val *)
(*                         FROM tab *)
(*                         WHERE tab.Id = {[r.Id]}); *)
(*     return <xml><body> *)
(*       cacheR {[r.FooBar]} *)
(*       {case res of *)
(*            None => <xml>?</xml> *)
(*          | Some row => <xml>{[row.Tab.Val]}</xml>} *)
(*     </body></xml> *)

(* fun cache2 id v = *)
(*     res <- oneOrNoRows (SELECT tab.Val *)
(*                         FROM tab *)
(*                         WHERE tab.Id = {[id]} AND tab.Val = {[v]}); *)
(*     return <xml><body> *)
(*       Reading {[id]}. *)
(*       {case res of *)
(*            None => <xml>Nope, that's not it.</xml> *)
(*          | Some _ => <xml>Hooray! You guessed it!</xml>} *)
(*     </body></xml> *)

(* fun cache2 id1 id2 = *)
(*     res1 <- oneOrNoRows (SELECT tab.Val *)
(*                          FROM tab *)
(*                          WHERE tab.Id = {[id1]}); *)
(*     res2 <- oneOrNoRows (SELECT tab.Val *)
(*                          FROM tab *)
(*                          WHERE tab.Id = {[id2]}); *)
(*     return <xml><body> *)
(*       Reading {[id1]} and {[id2]}. *)
(*       {case (res1, res2) of *)
(*            (Some _, Some _) => <xml>Both are there.</xml> *)
(*          | _ => <xml>One of them is missing.</xml>} *)
(*     </body></xml> *)

(* fun flash id = *)
(*     dml (UPDATE tab *)
(*          SET Foo = Val *)
(*          WHERE Id = {[id]} OR Id = {[id - 1]} OR Id = {[id + 1]}); *)
(*     return <xml><body> *)
(*       Maybe changed {[id]}? *)
(*     </body></xml> *)

(* fun floosh id = *)
(*     dml (UPDATE tab *)
(*          SET Id = {[id + 1]} *)
(*          WHERE Id = {[id]} OR Id = {[id - 1]} OR Id = {[id + 1]}); *)
(*     return <xml><body> *)
(*       Shifted {[id]}! *)
(*     </body></xml> *)

(* val flush17 = *)
(*     dml (UPDATE tab *)
(*          SET Val = Val * (Id + 2) / Val - 3 *)
(*          WHERE Id = 17); *)
(*     return <xml><body> *)
(*       Changed specifically 17! *)
(*     </body></xml> *)

(* fun flush id = *)
(*     res <- oneOrNoRows (SELECT tab.Val *)
(*                         FROM tab *)
(*                         WHERE tab.Id = {[id]}); *)
(*     (case res of *)
(*          None => dml (INSERT INTO tab (Id, Val) *)
(*                       VALUES ({[id]}, 0)) *)
(*        | Some row => dml (UPDATE tab *)
(*                           SET Val = {[row.Tab.Val + 1]} *)
(*                           WHERE Id = {[id]} OR Id = {[id + 1]})); *)
(*     return <xml><body> *)
(*       {case res of *)
(*            None => <xml>Initialized {[id]}!</xml> *)
(*          | Some row => <xml>Incremented {[id]}!</xml>} *)
(*     </body></xml> *)
