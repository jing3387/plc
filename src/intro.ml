open Core.Std

let rec lookup env x =
  match env with
  | [] -> None
  | (y, v) :: r -> if x = y then Some v else lookup r x

(* Exercise 1.1 *)
type expr =
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr

let rec eval e env =
  match e with
  | CstI i -> i
  | Var x ->
    (match lookup env x with
     | None -> failwith (x ^ " not found")
     | Some x -> x)
  | Prim (op, e1, e2) ->
    let e1' = eval e1 env in
    let e2' = eval e2 env in
    (match op with
     | "+" -> e1' + e2'
     | "*" -> e1' * e2'
     | "-" -> e1' * e2'
     | "==" -> if e1' = e2' then 1 else 0
     | "max" -> if e1' > e2' then e1' else e2'
     | "min" -> if e1' < e2' then e1' else e2'
     | _ -> failwith ("unknown primitive " ^ op))
  | If (e1, e2, e3) -> if eval e1 env <> 0 then eval e2 env else eval e3 env

(* Exercise 1.2.i *)
type aexpr =
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr

(* Exercise 1.2.ii *)
(* v - (w + z) *)
let e1 = Sub (Var "v", Add (Var "w", Var "z"))
(* 2 * (v - w + z) *)
let e2 = Mul (CstI 2, e1)
(* x + y + z + w *)
let e3 = Add (Add (Add (Var "x", Var "y"), Var "z"), Var "w")

(* Exercise 1.2.iii, Exercise 1.3 *)
(* Top-level for tracing. *)
let rec fmt' e n =
  let m, s =
    match e with
    | CstI i -> 3, string_of_int i
    | Var x -> 3, x
    | Mul (e1, e2) -> 2, fmt' e1 2 ^ " * " ^ fmt' e2 3
    | Add (e1, e2) -> 1, fmt' e1 1 ^ " + " ^ fmt' e2 2
    | Sub (e1, e2) -> 1, fmt' e1 1 ^ " - " ^ fmt' e2 2
  in if m < n then "(" ^ s ^ ")" else s

let fmt e = fmt' e (-1)

(* Exercise 1.2.iv *)
let rec simplify e =
  match e with
  | CstI _ | Var _ -> e
  | Mul (e1, e2)  ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    (match e1', e2' with
     | (CstI 1, _) -> e2'
     | (_, CstI 1) -> e1'
     | (CstI 0, _) -> CstI 0
     | (_, CstI 0) -> CstI 0
     | _ -> e)
  | Add (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    (match (e1', e2') with
     | (CstI 0, _) -> e2'
     | (_, CstI 0) -> e1'
     | _ -> e)
  | Sub (e1, e2) ->
    let e1' = simplify e1 in
    let e2' = simplify e2 in
    (match e1', e2' with
     | (_, CstI 0) -> e1'
     | _ -> if e1' = e2' then CstI 0 else e)
