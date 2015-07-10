open Core.Std

(* Evaluation, checking, and compilation of object language expressions. Stack
   machines for expression evaluation. *)

(** Object language expressions with variable bindings and nested scope. *)
type expr =
  | CstI of int
  | Var of string
  | Let of (string * expr) list * expr
  | Prim of string * expr * expr

(* Some closed expressions. *)

(* let z = 17 in z + z *)
let e1 = Let ([("z", CstI 17)], Prim ("+", Var "z", Var "z"))

(* let z = 17 in
   (let z = 22 in 100 * z) + z *)
let e2 =
  Let ([("z", CstI 17)],
    Prim ("+", Let ([("z", CstI 22)],
        Prim ("*", CstI 100, Var "z")), Var "z"))

(* let z = 5 - 4 in 100 * z *)
let e3 =
  Let ([("z", Prim ("-", CstI 5, CstI 4))],
    Prim ("*", CstI 100, Var "z"))

(* 20 + (let z = 17 in z + 2) + 30 *)
let e4 =
  Prim ("+", Prim ("+", CstI 20,
      Let ([("z", CstI 17)], Prim ("+", Var "z", CstI 2))),
    CstI 30)

(* 2 * (let x = 3 in x + 4) *)
let e5 = Prim ("*", CstI 2, Let ([("x", CstI 3)], Prim ("+", Var "x", CstI 4)))

(* let x1 = 5 + 7
       x2 = x1 * 2
   in x1 + x2 *)
let e6 =
  Let([("x1", (Prim ("+", CstI 5, CstI 7)));
       ("x2", (Prim ("*", Var "x1", CstI 2)))],
    (Prim ("+", Var "x1", Var "x2")))

(* x1 appears free in the rhs of x1 = x1 + 7. *)

(* let x = 11 in
   let x = 22
       y = x + 1
   in x + y *)
let e7 =
  Let ([("x", CstI 11)],
    (Let ([("x", CstI 22); ("y", (Prim ("+", Var "x", CstI 1)))],
       (Prim ("+", Var "x", Var "y")))))

(* ---------------------------------------------------------------------- *)

(* Evaluation. *)

(** Lookup variable x in the environment env. Fails with an exception if
    x is not in env. *)
let rec lookup_exn env x =
  match env with
  | [] -> failwith (x ^ " not found")
  | (y, v) :: r -> if x = y then v else lookup_exn r x

(** Evaluation of expressions with variables and bindings. *)
let rec eval e (env : (string * int) list) : int =
  match e with
  | CstI i -> i
  | Var x -> lookup_exn env x
  | Let (xs, ebody) ->
      let env' = List.map xs (fun (x, xval) -> (x, eval xval env))
      in eval ebody env'
  | Prim ("+", e1, e2) -> eval e1 env + eval e2 env
  | Prim ("*", e1, e2) -> eval e1 env * eval e2 env
  | Prim ("-", e1, e2) -> eval e1 env - eval e2 env
  | Prim (op, _, _) -> failwith ("unknown primitive" ^ op)

let run e = eval e []

(* ---------------------------------------------------------------------- *)

(* Free variables. *)

(* Operations on sets, represented as lists. Simple but inefficient; one could
   use binary trees, hashtables or splaytrees for efficiency. *)

let mem x vs = List.exists ~f:(fun y -> x = y) vs

let rec mem x vs =
  match vs with
  | [] -> false
  | v :: vr -> x = v || mem x vr

(** The set of all elements in xs or ys, without duplicates. *)
let rec union xs ys =
  match xs with
  | [] -> ys
  | x :: xr ->
      if mem x ys
      then union xr ys
      else x :: union xr ys

(** The set of all elements in xs but not in ys. *)
let rec minus xs ys =
  match xs with
  | []    -> []
  | x :: xr ->
      if mem x ys
      then minus xr ys
      else x :: minus xr ys

(** Find all variables that occur free in expression e. *)
let rec freevars e : string list =
  match e with
  | CstI i -> []
  | Var x  -> [x]
  | Let ([], ebody) -> freevars ebody
  | Let (xs, ebody) ->
      let freevars' =
        List.fold_right ~init:[] ~f:
          (fun (x, erhs) acc ->
            union (freevars erhs) (minus acc [x]))
      in
      let vars = List.map ~f:Tuple2.get1 in
      union (freevars' xs) (minus (freevars ebody) (vars xs))
  | Prim (ope, e1, e2) -> union (freevars e1) (freevars e2)

(** Alternative definition of closed. *)
let closed' e = (freevars e = [])
