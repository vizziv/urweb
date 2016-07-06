(* Copyright (c) 2008, Adam Chlipala
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * - The names of contributors may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

structure Prepare :> PREPARE = struct

open Cjr
open Settings

structure SM = BinaryMapFn(struct
                           type ord_key = string
                           val compare = String.compare
                           end)

structure St :> sig
    type t
    val empty : t
    val nameOf : t * string -> t * int
    val list : t -> (string * int) list
    val count : t -> int
end = struct

type t = {map : int SM.map, list : (string * int) list, count : int}

val empty = {map = SM.empty, list = [], count = 0}

fun nameOf (t as {map, list, count}, s) =
    case SM.find (map, s) of
        NONE => ({map = SM.insert (map, s, count), list = (s, count) :: list, count = count + 1}, count)
      | SOME n => (t, n)

fun list (t : t) = rev (#list t)
fun count (t : t) = #count t

end

fun patBinds env p =
    case #1 p of
        PVar (x, t) => NONE :: env
      | PPrim _ => env
      | PCon (_, _, NONE) => env
      | PCon (_, _, SOME p) => NONE :: env
      | PRecord xps => foldl (fn ((_, p, _), env) => patBinds env p) env xps
      | PNone _ => env
      | PSome (_, p) => NONE :: env

fun patBindsN p =
    case #1 p of
        PVar (x, t) => 1
      | PPrim _ => 0
      | PCon (_, _, NONE) => 0
      | PCon (_, _, SOME p) => 1
      | PRecord xps => foldl (fn ((_, p, _), acc) => patBindsN p + acc) 0 xps
      | PNone _ => 0
      | PSome (_, p) => 1

fun incERels bound i (e as (_, loc)) =
    let
        val incs = incERels bound i
        fun incsBind more = incERels (bound + more) i
    in
        case #1 e of
            EPrim _ => e
          | ERel n => (ERel (if n < bound then n else n + i), loc)
          | ENamed _ => e
          | ECon (dk, pc, eo) => (ECon (dk, pc, Option.map incs eo), loc)
          | ENone _ => e
          | ESome (t, e) => (ESome (t, incs e), loc)
          | EFfi _ => e
          | EFfiApp (m, x, ets) => (EFfiApp (m, x, map (fn (e, t) => (incs e, t)) ets), loc)
          | EApp (e, es) => (EApp (incs e, map incs es), loc)
          | EUnop (s, e) => (EUnop (s, incs e), loc)
          | EBinop (s, e1, e2) => (EBinop (s, incs e1, incs e2), loc)
          | ERecord (n, fes) => (ERecord (n, map (fn (f, e) => (f, incs e)) fes), loc)
          | EField (e, f) => (EField (incs e, f), loc)
          | ECase (e, pes, ts) => (ECase (incs e, map (fn (p, e) => (p, incsBind (patBindsN p) e)) pes, ts), loc)
          | EError (e, s) => (EError (incs e, s), loc)
          | EReturnBlob {blob = eo, mimeType = e, t} => (EReturnBlob {blob = Option.map incs eo, mimeType = incs e, t = t}, loc)
          | ERedirect (e, t) => (ERedirect (incs e, t), loc)
          | EWrite e => (EWrite (incs e), loc)
          | ESeq (e1, e2) => (ESeq (incs e1, incs e2), loc)
          | ELet (x, t, e1, e2) => (ELet (x, t, incs e1, incsBind 1 e2), loc)
          | EQuery _ => raise Fail "Prepare.incERels: Ziv is tired and this EQuery case probably shouldn't happen anyway"
          | EDml _ => raise Fail "Prepare.incERels: Ziv is tired and this EDml case probably shouldn't happen anyway"
          | ENextval _ => raise Fail "Prepare.incERels: Ziv is tired and this ENextval case probably shouldn't happen anyway"
          | ESetval _ => raise Fail "Prepare.incERels: Ziv is tired and this ESetval case probably shouldn't happen anyway"
          | EUnurlify (e, t, b) => (EUnurlify (incs e, t, b), loc)
    end

fun envLookup env n = Option.map (incERels 0 (n + 1)) (List.nth (env, n))

fun prepString env (e, st) =
    let
        fun prepString' (eAll, ss, n) =
            let
                fun doOne t =
                    SOME (#p_blank (Settings.currentDbms ()) (n + 1, t) :: ss, n + 1, eAll)
            in
                case #1 eAll of
                    EPrim (Prim.String (_, s)) =>
                    SOME (s :: ss, n, eAll)

                  (* Environment lookup. *)
                  | ERel i => Option.mapPartial (prepString' o (fn e => (e, ss, n))) (envLookup env i)

                  | EFfiApp ("Basis", "strcat", [(e1, t1), (e2, t2)]) =>
                    (case prepString' (e1, ss, n) of
                         NONE => NONE
                       | SOME (ss, n, e1) =>
                         case prepString' (e2, ss, n) of
                             NONE => NONE
                           | SOME (ss, n, e2) => SOME (ss, n, (EFfiApp ("Basis", "strcat", [(e1, t1), (e2, t2)]), #2 eAll)))
                  | EFfiApp ("Basis", "sqlifyInt", [_]) => doOne Int
                  | EFfiApp ("Basis", "sqlifyFloat", [_]) => doOne Float
                  | EFfiApp ("Basis", "sqlifyString", [_]) => doOne String
                  | EFfiApp ("Basis", "sqlifyBool", [_]) => doOne Bool
                  | EFfiApp ("Basis", "sqlifyTime", [_]) => doOne Time
                  | EFfiApp ("Basis", "sqlifyBlob", [_]) => doOne Blob
                  | EFfiApp ("Basis", "sqlifyChannel", [_]) => doOne Channel
                  | EFfiApp ("Basis", "sqlifyClient", [_]) => doOne Client

                  | ECase (e,
                           [((PNone _, _),
                             (EPrim (Prim.String (_, "NULL")), _)),
                            ((PSome (_, (PVar _, _)), _),
                             (EFfiApp (m, x, [((ERel 0, _), _)]), _))],
                           {disc = t, ...}) =>
                    (* The recursive call will be a doOne, so we just replace its return expression with eAll. *)
                    (case prepString' ((EFfiApp (m, x, [(e, t)]), #2 e), ss, n) of
                         NONE => NONE
                       | SOME (ss, n, _) => SOME (ss, n, eAll))
                  | ECase (e,
                           [((PCon (_, PConFfi {mod = "Basis", con = "True", ...}, _), _),
                             (EPrim (Prim.String (_, "TRUE")), _)),
                            ((PCon (_, PConFfi {mod = "Basis", con = "False", ...}, _), _),
                             (EPrim (Prim.String (_, "FALSE")), _))],
                           _) => doOne Bool

                  | _ => NONE
            end
    in
        case prepString' (e, [], 0) of
            NONE => NONE
          | SOME (ss, n, e) =>
            let
                val s = String.concat (rev ss)
                val (st, id) = St.nameOf (st, s)
            in
                SOME (id, s, st, e)
            end
    end

fun prepExp env (e as (_, loc), st) =
    case #1 e of
        EPrim _ => (e, st)
      | ERel _ => (e, st)
      | ENamed _ => (e, st)
      | ECon (_, _, NONE) => (e, st)
      | ECon (dk, pc, SOME e) =>
        let
            val (e, st) = prepExp env (e, st)
        in
            ((ECon (dk, pc, SOME e), loc), st)
        end
      | ENone t => (e, st)
      | ESome (t, e) =>
        let
            val (e, st) = prepExp env (e, st)
        in
            ((ESome (t, e), loc), st)
        end
      | EFfi _ => (e, st)
      | EFfiApp (m, x, es) =>
        let
            val (es, st) = ListUtil.foldlMap (fn ((e, t), st) =>
                                                 let
                                                     val (e, st) = prepExp env (e, st)
                                                 in
                                                     ((e, t), st)
                                                 end) st es
        in
            ((EFfiApp (m, x, es), loc), st)
        end
      | EApp (e1, es) =>
        let
            val (e1, st) = prepExp env (e1, st)
            val (es, st) = ListUtil.foldlMap (prepExp env) st es
        in
            ((EApp (e1, es), loc), st)
        end

      | EUnop (s, e1) =>
        let
            val (e1, st) = prepExp env (e1, st)
        in
            ((EUnop (s, e1), loc), st)
        end
      | EBinop (s, e1, e2) =>
        let
            val (e1, st) = prepExp env (e1, st)
            val (e2, st) = prepExp env (e2, st)
        in
            ((EBinop (s, e1, e2), loc), st)
        end

      | ERecord (rn, xes) =>
        let
            val (xes, st) = ListUtil.foldlMap (fn ((x, e), st) =>
                                                   let
                                                       val (e, st) = prepExp env (e, st)
                                                   in
                                                       ((x, e), st)
                                                   end) st xes
        in
            ((ERecord (rn, xes), loc), st)
        end
      | EField (e, s) =>
        let
            val (e, st) = prepExp env (e, st)
        in
            ((EField (e, s), loc), st)
        end

      | ECase (e, pes, ts) =>
        let
            val (e, st) = prepExp env (e, st)
            val (pes, st) = ListUtil.foldlMap (fn ((p, e), st) =>
                                                   let
                                                       val (e, st) = prepExp (patBinds env p) (e, st)
                                                   in
                                                       ((p, e), st)
                                                   end) st pes
        in
            ((ECase (e, pes, ts), loc), st)
        end

      | EError (e, t) =>
        let
            val (e, st) = prepExp env (e, st)
        in
            ((EError (e, t), loc), st)
        end

      | EReturnBlob {blob, mimeType, t} =>
        let
            val (blob, st) = case blob of
                                 NONE => (blob, st)
                               | SOME blob =>
                                 let
                                     val (b, st) = prepExp env (blob, st)
                                 in
                                     (SOME b, st)
                                 end
            val (mimeType, st) = prepExp env (mimeType, st)
        in
            ((EReturnBlob {blob = blob, mimeType = mimeType, t = t}, loc), st)
        end

      | ERedirect (e, t) =>
        let
            val (e, st) = prepExp env (e, st)
        in
            ((ERedirect (e, t), loc), st)
        end

      | EWrite e =>
        let
            val (e, st) = prepExp env (e, st)
        in
            ((EWrite e, loc), st)
        end
      | ESeq (e1, e2) =>
        let
            val (e1, st) = prepExp env (e1, st)
            val (e2, st) = prepExp env (e2, st)
        in
            ((ESeq (e1, e2), loc), st)
        end
      | ELet (x, t, e1, e2) =>
        let
            val (e1, st) = prepExp env (e1, st)
            val (e2, st) = prepExp (SOME e1 :: env) (e2, st)
        in
            ((ELet (x, t, e1, e2), loc), st)
        end

      | EQuery {exps, tables, rnum, state, query, body, initial, ...} =>
        let
            val (body, st) = prepExp env (body, st)
        in
            case prepString env (query, st) of
                NONE =>
                ((EQuery {exps = exps, tables = tables, rnum = rnum,
                          state = state, query = query, body = body,
                          initial = initial, prepared = NONE}, loc),
                 st)
              | SOME (id, s, st, query) =>
                ((EQuery {exps = exps, tables = tables, rnum = rnum,
                          state = state, query = query, body = body,
                          initial = initial, prepared = SOME {id = id, query = s, nested = true}}, loc), st)
        end

      | EDml {dml, mode, ...} =>
        (case prepString env (dml, st) of
             NONE => (e, st)
           | SOME (id, s, st, dml) =>
             ((EDml {dml = dml, prepared = SOME {id = id, dml = s}, mode = mode}, loc), st))

      | ENextval {seq, ...} =>
        if #supportsNextval (Settings.currentDbms ()) then
            let
                val s = case seq of
                            (EPrim (Prim.String (_, s)), loc) =>
                            (EPrim (Prim.String (Prim.Normal, "SELECT NEXTVAL('" ^ s ^ "')")), loc)
                          | _ =>
                            let
                                val t = (TFfi ("Basis", "string"), loc)
                                val s' = (EFfiApp ("Basis", "strcat", [(seq, t), ((EPrim (Prim.String (Prim.Normal, "')")), loc), t)]), loc)
                            in
                                (EFfiApp ("Basis", "strcat", [((EPrim (Prim.String (Prim.Normal, "SELECT NEXTVAL('")), loc), t), (s', t)]), loc)
                            end
            in
                case prepString env (s, st) of
                    NONE => (e, st)
                  | SOME (id, s, st, seq) =>
                    ((ENextval {seq = seq, prepared = SOME {id = id, query = s}}, loc), st)
            end
        else
            (e, st)

      | ESetval {seq = e1, count = e2} =>
        let
            val (e1, st) = prepExp env (e1, st)
            val (e2, st) = prepExp env (e2, st)
        in
            ((ESetval {seq = e1, count = e2}, loc), st)
        end

      | EUnurlify (e, t, b) =>
        let
            val (e, st) = prepExp env (e, st)
        in
            ((EUnurlify (e, t, b), loc), st)
        end

fun prepDecl (d as (_, loc), st) =
    case #1 d of
        DStruct _ => (d, st)
      | DDatatype _ => (d, st)
      | DDatatypeForward _ => (d, st)
      | DVal (x, n, t, e) =>
        let
            val (e, st) = prepExp [] (e, st)
        in
            ((DVal (x, n, t, e), loc), st)
        end
      | DFun (x, n, xts, t, e) =>
        let
            val (e, st) = prepExp [] (e, st)
        in
            ((DFun (x, n, xts, t, e), loc), st)
        end
      | DFunRec fs =>
        let
            val (fs, st) = ListUtil.foldlMap (fn ((x, n, xts, t, e), st) =>
                                                  let
                                                      val (e, st) = prepExp [] (e, st)
                                                  in
                                                      ((x, n, xts, t, e), st)
                                                  end) st fs
        in
            ((DFunRec fs, loc), st)
        end

      | DTable _ => (d, st)
      | DSequence _ => (d, st)
      | DView _ => (d, st)
      | DDatabase _ => (d, st)
      | DPreparedStatements _ => (d, st)
      | DJavaScript _ => (d, st)
      | DCookie _ => (d, st)
      | DStyle _ => (d, st)
      | DTask (tk, x1, x2, e) =>
        let
            val (e, st) = prepExp [] (e, st)
        in
            ((DTask (tk, x1, x2, e), loc), st)
        end
      | DOnError _ => (d, st)

fun prepare (ds, ps) =
    let
        val (ds, st) = ListUtil.foldlMap prepDecl St.empty ds
    in
        ((DPreparedStatements (St.list st), ErrorMsg.dummySpan) :: ds, ps)
    end

end
