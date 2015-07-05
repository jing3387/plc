open Core.Std

let rec lookup env x =
  match env with
  | [] -> None
  | (y, v) :: r -> if x = y then Some v else lookup r x

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
