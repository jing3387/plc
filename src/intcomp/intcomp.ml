open Core.Std

(* Evaluation, checking, and compilation of object language expressions. Stack
   machines for expression evaluation. *)

(** Object language expressions with variable bindings and nested scope. *)
type expr =
  | CstI of int
  | Var of string
  | Let of string * expr * expr
  | Prim of string * expr * expr

(* Some closed expressions. *)

let e1 = Let ("z", CstI 17, Prim ("+", Var "z", Var "z"))

let e2 =
  Let ("z", CstI 17,
    Prim ("+", Let ("z", CstI 22, Prim ("*", CstI 100, Var "z")),
      Var "z"))

let e3 =
  Let ("z", Prim ("-", CstI 5, CstI 4),
    Prim ("*", CstI 100, Var "z"))

let e4 =
  Prim ("+", Prim ("+", CstI 20, Let ("z", CstI 17,
        Prim ("+", Var "z", CstI 2))),
    CstI 30)

let e5 = Prim ("*", CstI 2, Let ("x", CstI 3, Prim ("+", Var "x", CstI 4)))

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
  | Let (x, erhs, ebody) ->
      let xval = eval erhs env in
      let env' = (x, xval) :: env in
      eval ebody env'
  | Prim ("+", e1, e2) -> eval e1 env + eval e2 env
  | Prim ("*", e1, e2) -> eval e1 env * eval e2 env
  | Prim ("-", e1, e2) -> eval e1 env - eval e2 env
  | Prim (op, _, _) -> failwith ("unknown primitive" ^ op)

let run e = eval e []

(* ---------------------------------------------------------------------- *)

(* Closedness *)

let mem x vs = List.exists ~f:(fun y -> x = y) vs

let rec mem x vs =
  match vs with
  | [] -> false
  | v :: vr -> x = v || mem x vr

(** Checking whether an expression is closed. vs is a list of the bound
    variables. *)
let rec closed_in (e : expr) (vs : string list) : bool =
  match e with
  | CstI i -> true
  | Var x -> List.exists ~f:(fun y -> x = y) vs
  | Let (x, erhs, ebody) ->
      let vs' = x :: vs
      in closed_in erhs vs && closed_in ebody vs'
  | Prim (op, e1, e2) -> closed_in e1 vs && closed_in e2 vs

(** An expression is closed if it is closed in the empty environment *)
let closed e = closed_in e []

(* ---------------------------------------------------------------------- *)

(* Substitution of expressions for variables. *)

(** This version of lookup returns a Var x expression if there is no pair
    (x, e) in the list env instead of failing with an exception. *)
let rec lookup env x =
  match env with
  | [] -> Var x
  | (y, e) :: r -> if x = y then e else lookup r x

(** Remove (x, _) from env. *)
let rec remove env x =
  match env with
  | [] -> []
  | (y, e) :: xs -> if x = y then xs else (y, e) :: remove xs x

(** Naive substitution, may capture free variables. *)
let rec nsubst (e : expr) (env : (string * expr) list) : expr =
  match e with
  | CstI _ -> e
  | Var x  -> lookup env x
  | Let (x, erhs, ebody) ->
      let newenv = remove env x in
      Let (x, nsubst erhs env, nsubst ebody newenv)
  | Prim (op, e1, e2) -> Prim (op, nsubst e1 env, nsubst e2 env)

(* Some expressions with free variables. *)
let e6 = Prim ("+", Var "y", Var "z")

let e6s1 = nsubst e6 [("z", CstI 17)]

let e6s2 = nsubst e6 [("z", Prim ("-", CstI 5, CstI 4))]

let e6s3 = nsubst e6 [("z", Prim ("+", Var "z", Var "z"))]

(* Shows that only z outside the Let gets substituted. *)
let e7 = Prim ("+", Let ("z", CstI 22, Prim ("*", CstI 5, Var "z")),
    Var "z")

let e7s1 = nsubst e7 [("z", CstI 100)]

(* Shows that only the z in the Let rhs gets substituted. *)
let e8 = Let ("z", Prim ("*", CstI 22, Var "z"), Prim ("*", CstI 5, Var "z"))

let e8s1 = nsubst e8 [("z", CstI 100)]

(* Shows (wrong) capture of free variable z under the let. *)
let e9 = Let ("z", CstI 22, Prim ("*", Var "y", Var "z"))

let e9s1 = nsubst e9 [("y", Var "z")]

let e9s2 = nsubst e9 [("z", Prim ("-", CstI 5, CstI 4))]

let new_var : string -> string =
  let n = ref 0 in
  let make_var x = (n := 1 + !n; x ^ string_of_int !n) in
  make_var

(** Correct, capture-avoiding substitution. *)
let rec subst (e : expr) (env : (string * expr) list) : expr =
  match e with
  | CstI i -> e
  | Var x  -> lookup env x
  | Let (x, erhs, ebody) ->
      let newx = new_var x in
      let newenv = (x, Var newx) :: remove env x in
      Let (newx, subst erhs env, subst ebody newenv)
  | Prim (op, e1, e2) -> Prim (op, subst e1 env, subst e2 env)

let e6s1a = subst e6 [("z", CstI 17)]

let e6s2a = subst e6 [("z", Prim ("-", CstI 5, CstI 4))]

let e6s3a = subst e6 [("z", Prim ("+", Var "z", Var "z"))]

(* Shows renaming of bound variable z (to z1). *)
let e7s1a = subst e7 [("z", CstI 100)]

(* Shows renaming of bound variable z (to z2). *)
let e8s1a = subst e8 [("z", CstI 100)]

(* Shows renaming of bound variable z (to z3), avoiding capture of free z. *)
let e9s1a = subst e9 [("y", Var "z")]

(* ---------------------------------------------------------------------- *)

(* Free variables. *)

(* Operations on sets, represented as lists. Simple but inefficient; one could
   use binary trees, hashtables or splaytrees for efficiency. *)

(** The set of all elements in xs or ys, without duplicates. *)
let rec union (xs, ys) =
  match xs with
  | [] -> ys
  | x :: xr ->
      if mem x ys
      then union (xr, ys)
      else x :: union (xr, ys)

(** The set of all elements in xs but not in ys. *)
let rec minus (xs, ys) =
  match xs with
  | []    -> []
  | x :: xr ->
      if mem x ys
      then minus(xr, ys)
      else x :: minus (xr, ys)

(** Find all variables that occur free in expression e. *)
let rec freevars e : string list =
  match e with
  | CstI i -> []
  | Var x  -> [x]
  | Let (x, erhs, ebody) ->
      union (freevars erhs, minus (freevars ebody, [x]))
  | Prim (ope, e1, e2) -> union (freevars e1, freevars e2)

(** Alternative definition of closed. *)
let closed' e = (freevars e = [])

(* ---------------------------------------------------------------------- *)

(* Target expressions. *)

(** Compilation to target expressions with numerical indexes instead of
    symbolic variable names. *)
type texpr =
  | TCstI of int
  | TVar of int                     (* Index into runtime environment. *)
  | TLet of texpr * texpr           (* erhs and ebody. *)
  | TPrim of string * texpr * texpr


(** Map variable name to variable index at compile-time. *)
let rec getindex vs x =
  match vs with
  | [] -> failwith "variable not found"
  | y :: ys -> if x = y then 0 else 1 + getindex ys x

(** Compiling from expr to texpr. *)
let rec tcomp (e : expr) (cenv : string list) : texpr =
  match e with
  | CstI i -> TCstI i
  | Var x  -> TVar (getindex cenv x)
  | Let (x, erhs, ebody) ->
      let cenv' = x :: cenv in
      TLet (tcomp erhs cenv, tcomp ebody cenv')
  | Prim (op, e1, e2) -> TPrim (op, tcomp e1 cenv, tcomp e2 cenv)

(** Evaluation of target expressions with variable indexes. The run-time
    environment renv is a list of variable values (ints). *)
let rec teval (e : texpr) (renv : int list) : int =
  match e with
  | TCstI i -> i
  | TVar n  -> List.nth_exn renv n
  | TLet (erhs, ebody) ->
      let xval = teval erhs renv in
      let renv1 = xval :: renv in
      teval ebody renv1
  | TPrim ("+", e1, e2) -> teval e1 renv + teval e2 renv
  | TPrim ("*", e1, e2) -> teval e1 renv * teval e2 renv
  | TPrim ("-", e1, e2) -> teval e1 renv - teval e2 renv
  | TPrim (op, _, _) -> failwith ("unknown primitive" ^ op)

(* Correctness: eval e []  equals  teval (tcomp e []) [] *)

(* ---------------------------------------------------------------------- *)

(* Stack machines. *)

(** Stack machine instructions. An expression in postfix or reverse Polish
    notation is a list of stack machine instructions. *)
type rinstr =
  | RCstI of int
  | RAdd
  | RSub
  | RMul
  | RDup
  | RSwap

(** A simple stack machine for evaluation of variable-free expressions in
    postfix form *)
let rec reval (inss : rinstr list) (stack : int list) : int =
  match (inss, stack) with
  | ([], v :: _) -> v
  | ([], []) -> failwith "reval: no result on stack!"
  | (RCstI i :: insr, stk) -> reval insr (i :: stk)
  | (RAdd :: insr, i2 :: i1 :: stkr) -> reval insr ((i1+i2) :: stkr)
  | (RSub :: insr, i2 :: i1  ::  stkr) -> reval insr ((i1-i2) :: stkr)
  | (RMul :: insr, i2 :: i1  ::  stkr) -> reval insr ((i1*i2) :: stkr)
  | (RDup :: insr, i1 :: stkr) -> reval insr (i1 :: i1 :: stkr)
  | (RSwap :: insr, i2 :: i1 :: stkr) -> reval insr (i1 :: i2 :: stkr)
  | _ -> failwith "reval: too few operands on stack"

let rpn1 = reval [RCstI 10; RCstI 17; RDup; RMul; RAdd] []

(** Compilation of a variable-free expression to a rinstr list *)
let rec rcomp (e : expr) : rinstr list =
  match e with
  | CstI i -> [RCstI i]
  | Var _ -> failwith "rcomp cannot compile Var"
  | Let _ -> failwith "rcomp cannot compile Let"
  | Prim ("+", e1, e2) -> rcomp e1 @ rcomp e2 @ [RAdd]
  | Prim ("*", e1, e2) -> rcomp e1 @ rcomp e2 @ [RMul]
  | Prim ("-", e1, e2) -> rcomp e1 @ rcomp e2 @ [RSub]
  | Prim (op, _, _) -> failwith ("unknown primitive " ^ op)

(* Correctness: eval e []  equals  reval (rcomp e) [] *)

(** Storing intermediate results and variable bindings in the same stack *)
type sinstr =
  | SCstI of int (* push integer. *)
  | SVar of int  (* push variable from env. *)
  | SAdd         (* pop args, push sum. *)
  | SSub         (* pop args, push diff. *)
  | SMul         (* pop args, push product. *)
  | SPop         (* pop value/unbind var. *)
  | SSwap        (* exchange top and next. *)

let rec seval (inss : sinstr list) (stack : int list) =
  match (inss, stack) with
  | ([], v :: _) -> v
  | ([], [])     -> failwith "seval: no result on stack"
  | (SCstI i :: insr, stk) -> seval insr (i :: stk)
  | (SVar i :: insr, stk) -> seval insr (List.nth_exn stk i :: stk)
  | (SAdd :: insr, i2 :: i1 :: stkr) -> seval insr (i1 + i2 :: stkr)
  | (SSub :: insr, i2 :: i1 :: stkr) -> seval insr (i1 - i2 :: stkr)
  | (SMul :: insr, i2 :: i1 :: stkr) -> seval insr (i1 * i2 :: stkr)
  | (SPop :: insr, _  ::  stkr) -> seval insr stkr
  | (SSwap :: insr, i2 :: i1 :: stkr) -> seval insr (i1 :: i2 :: stkr)
  | _ -> failwith "seval: too few operands on stack"

(** A compile-time variable environment representing the state of the run-time
    stack. *)
type stackvalue =
  | Value           (* A computed value *)
  | Bound of string (* A bound variable *)

(** Compilation to a list of instructions for a unified-stack machine. *)
let rec scomp (e : expr) (cenv : stackvalue list) : sinstr list =
  match e with
  | CstI i -> [SCstI i]
  | Var x  -> [SVar (getindex cenv (Bound x))]
  | Let (x, erhs, ebody) ->
      scomp erhs cenv @ scomp ebody (Bound x :: cenv) @ [SSwap; SPop]
  | Prim ("+", e1, e2) ->
      scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SAdd]
  | Prim ("-", e1, e2) ->
      scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SSub]
  | Prim ("*", e1, e2) ->
      scomp e1 cenv @ scomp e2 (Value :: cenv) @ [SMul]
  | Prim (op, _, _) -> failwith ("scomp: unknown operator " ^ op)

let s1 = scomp e1 []
let s2 = scomp e2 []
let s3 = scomp e3 []
let s5 = scomp e5 []

(** Output the integers in list inss to the text file called fname: *)
let intsToFile (inss : int list) (fname : string) =
  let text = String.concat ~sep:" " (List.map ~f:string_of_int inss) in
  Out_channel.write_all fname text
