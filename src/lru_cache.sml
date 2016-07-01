structure LruCache : sig
    val cache : Cache.cache
end = struct


(* Mono *)

open Mono

val dummyLoc = ErrorMsg.dummySpan
fun ffiTyp name = (TFfi ("Basis", name), dummyLoc)
val stringTyp = ffiTyp "string"
val optionStringTyp = (TOption stringTyp, dummyLoc)
fun withTyp typ = map (fn exp => (exp, typ))

fun ffiPatBool b =
    (PCon (Enum,
           PConFfi {mod = "Basis",
                    datatyp = "bool",
                    con = if b then "True" else "False",
                    arg = NONE},
           NONE),
     dummyLoc)

fun ffiAppCache' (func, index, argTyps) =
    EFfiApp ("Sqlcache", func ^ Int.toString index, argTyps)

fun check (index, keys) =
    ffiAppCache' ("check", index, withTyp stringTyp keys)

fun store (index, keys, value) =
    let
        fun ffiStore v =
            (ffiAppCache' ("store", index, (v, optionStringTyp) :: withTyp stringTyp keys),
             dummyLoc)
    in
        ECase ((ffiAppCache' ("isDeactivated", index, []), dummyLoc),
               [(ffiPatBool true, ffiStore (ENone stringTyp, dummyLoc)),
                (ffiPatBool false, ffiStore (ESome (stringTyp, value), dummyLoc))],
               {disc = ffiTyp "bool", result = ffiTyp "unit"})
    end

fun flush (index, keys) =
    ffiAppCache' ("flush", index, withTyp optionStringTyp keys)

fun lock (index, write) =
    ffiAppCache' ((if write then "w" else "r") ^ "lock", index, [])


(* Cjr *)

open Print
open Print.PD

fun setupQuery {index, keyLevels} =
    let
        val i = Int.toString index

        val numKeysInLevel = map length keyLevels

        val numLevels = length keyLevels

        val numKeysTotal = List.foldl op+ 0 numKeysInLevel

        fun mapSep f sep =
         fn [] => ""
          | [x] => f x
          | (x :: xs) => f x ^ sep ^ mapSep f sep xs

        fun mapSepInit itemi sep =
         fn [] => ""
          | xs => sep ^ mapSep itemi sep xs

        fun paramRepeat f sep = mapSep f sep (List.tabulate (numKeysTotal, Int.toString))

        fun paramRepeatInit f sep = mapSepInit f sep (List.tabulate (numKeysTotal, Int.toString))

        fun showList f xs = "{" ^ mapSep f ", " xs ^ "}"

        val typedArgs = paramRepeatInit (fn p => "uw_Basis_string p" ^ p) ", "

        val args = paramRepeat (fn p => "p" ^ p) ", "

        val argNums = List.tabulate (numKeysTotal, fn i => "p" ^ Int.toString i)

        fun doKeyLevel (n, level) =
            [string ("size_t keyLevel" ^ i ^ "_" ^ Int.toString n ^ " = "
                     ^ showList Int.toString level ^ ";"),
             newline]
    in
        Print.box
            [string ("static uw_Sqlcache_Cache cacheStruct" ^ i ^ " = {"),
             newline,
             string "  .lockActivation = PTHREAD_RWLOCK_INITIALIZER,",
             newline,
             string "  .lockOut = PTHREAD_RWLOCK_INITIALIZER,",
             newline,
             string "  .lockIn = PTHREAD_RWLOCK_INITIALIZER,",
             newline,
             string "  .table = NULL,",
             newline,
             string "  .timeInvalid = 0,",
             newline,
             string "  .timeRefreshed = 0,",
             newline,
             (* Anything inserted at the beginning is valid, so start at 1, not 0. *)
             string "  .timeNow = 1,",
             newline,
             string ("  .keyLevels = " ^ showList (showList Int.toString) keyLevels ^ ","),
             newline,
             string ("  .numKeysInLevel = " ^ showList Int.toString numKeysInLevel ^ ","),
             newline,
             string ("  .numLevels = " ^ Int.toString numLevels ^ ","),
             newline,
             string ("  .numKeysTotal = " ^ Int.toString numKeysTotal ^ ","),
             newline,
             string "  .hitRatio = 1.0,",
             newline,
             string "  .inLimbo = 0,",
             newline,
             string "  .isDeactivated = 0};",
             newline,
             string ("static uw_Sqlcache_Cache *cache" ^ i ^ " = &cacheStruct" ^ i ^ ";"),
             newline,
             newline,

             string ("static uw_Basis_bool uw_Sqlcache_isDeactivated" ^ i ^ "(uw_context ctx) {"),
             newline,
             string ("  return cache" ^ i ^ "->isDeactivated;"),
             newline,
             string "}",
             newline,
             newline,

             string ("static void uw_Sqlcache_rlock" ^ i ^ "(uw_context ctx) {"),
             newline,
             string ("  uw_Sqlcache_rlock(ctx, cache" ^ i ^ ");"),
             newline,
             string "}",
             newline,
             newline,

             string ("static void uw_Sqlcache_wlock" ^ i ^ "(uw_context ctx) {"),
             newline,
             string ("  uw_Sqlcache_wlock(ctx, cache" ^ i ^ ");"),
             newline,
             string "}",
             newline,
             newline,

             string ("static uw_Basis_string uw_Sqlcache_check" ^ i
                     ^ "(uw_context ctx" ^ typedArgs ^ ") {"),
             newline,
             string ("  char *ks[] = {" ^ args ^ "};"),
             newline,
             string ("  uw_Sqlcache_Value *v = uw_Sqlcache_check(ctx, cache" ^ i ^ ", ks);"),
             newline,
             (* If the output is null, it means we had too much recursion, so it's a miss. *)
             string "  if (v && v->output != NULL) {",
             newline,
             (* DEBUG *)
             (* string ("    puts(\"SQLCACHE: hit " ^ i ^ ".\");"), *)
             (* newline, *)
             (* /DEBUG *)
             string "    uw_write(ctx, v->output);",
             newline,
             string "    uw_write_script(ctx, v->scriptOutput);",
             newline,
             string "    return v->result;",
             newline,
             string "  } else {",
             newline,
             (* DEBUG *)
             (* string ("    printf(\"SQLCACHE: miss " ^ i ^ " " ^ String.concatWith ", " (List.tabulate (numKeysTotal, fn _ => "%s")) ^ ".\\n\""), *)
             (* (case argNums of *)
             (*      [] => Print.box [] *)
             (*     | _ => Print.box [string ", ", *)
             (*                       p_list string argNums]), *)
             (* string ");", *)
             (* newline, *)
             (* /DEBUG *)
             string "    uw_recordingStart(ctx);",
             newline,
             string "    return NULL;",
             newline,
             string "  }",
             newline,
             string "}",
             newline,
             newline,

             string ("static uw_unit uw_Sqlcache_store" ^ i
                     ^ "(uw_context ctx, uw_Basis_string s" ^ typedArgs ^ ") {"),
             newline,
             string ("  char *ks[] = {" ^ args ^ "};"),
             newline,
             string ("  uw_Sqlcache_Value *v = calloc(1, sizeof(uw_Sqlcache_Value));"),
             newline,
             (* [s] is null if and only if the cache is deactivated, in which
                case all of these three end up null because of [calloc]. *)
             string "  if(s) {",
             newline,
             string "    v->result = strdup(s);",
             newline,
             string "    v->output = uw_recordingRead(ctx);",
             newline,
             string "    v->scriptOutput = uw_recordingReadScript(ctx);",
             newline,
             string "  }",
             newline,
             (* DEBUG *)
             (* string ("  puts(\"SQLCACHE: stored " ^ i ^ ".\");"), *)
             (* newline, *)
             (* /DEBUG *)
             string ("  uw_Sqlcache_store(ctx, cache" ^ i ^ ", ks, v);"),
             newline,
             string "  return uw_unit_v;",
             newline,
             string "}",
             newline,
             newline,

             string ("static uw_unit uw_Sqlcache_flush" ^ i
                     ^ "(uw_context ctx" ^ typedArgs ^ ") {"),
             newline,
             string ("  char *ks[] = {" ^ args ^ "};"),
             newline,
             string ("  uw_Sqlcache_flush(ctx, cache" ^ i ^ ", ks);"),
             newline,
             (* DEBUG *)
             (* string ("  puts(\"SQLCACHE: flushed " ^ i ^ ".\");"), *)
             (* newline, *)
             (* /DEBUG *)
             string "  return uw_unit_v;",
             newline,
             string "}",
             newline,
             newline]
    end

fun setupGlobal () =
    let
        val bit = if Settings.getSqlcacheAutotuning () then "1" else "0"
    in
        string ("int uw_Sqlcache_amAutotuning = " ^ bit ^ ";")
    end

(* Bundle it up. *)
val cache =
    {check = check, store = store, flush = flush, lock = lock,
     setupQuery = setupQuery, setupGlobal = setupGlobal}

end
