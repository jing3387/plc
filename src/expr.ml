open Core.Std

let rec lookup env x =
  match env with
  | [] -> None
  | (y, v) :: r -> if x = y then Some v else lookup r x

type expr =
  | CstI of int
  | Var of string
  | Prim of string * expr * expr

let rec eval e env =
  match e with
  | CstI i            -> i
  | Var x             ->
      (match lookup env x with
       | None -> failwith (x ^ " not found")
       | Some x -> x)
  | Prim ("+", e1, e2) -> eval e1 env + eval e2 env
  | Prim ("*", e1, e2) -> eval e1 env * eval e2 env
  | Prim ("-", e1, e2) -> eval e1 env - eval e2 env
  | Prim (op, _, _)     -> failwith ("unknown primitive " ^ op)
