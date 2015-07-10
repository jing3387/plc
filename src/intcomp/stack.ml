open Core.Std

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

(** Map variable name to variable index at compile-time. *)
let rec getindex vs x =
  match vs with
  | [] -> failwith "variable not found"
  | y :: ys -> if x = y then 0 else 1 + getindex ys x

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

let rec assemble (inss : sinstr list) : int list =
  match inss with
  | [] -> []
  | (SCstI i :: insr) -> 0 :: i :: assemble insr
  | (SVar x :: insr) -> 1 :: x :: assemble insr
  | (SAdd :: insr) -> 2 :: assemble insr
  | (SSub :: insr) -> 3 :: assemble insr
  | (SMul :: insr) -> 4 :: assemble insr
  | (SPop :: insr) -> 5 :: assemble insr
  | (SSwap :: insr) -> 6 :: assemble insr

(** Output the integers in list inss to the text file called fname: *)
let intsToFile (inss : int list) (fname : string) =
  let outc = Out_channel.create fname in
  List.iter inss ~f:(fun x -> Out_channel.output_binary_int outc x);
  Out_channel.close outc
