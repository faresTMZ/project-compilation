(* Interprète Mini-ML *)

open Mml
open Option

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make(String)

(* Valeurs *)
type value =
  | VInt   of int
  | VBool  of bool
  | VUnit
  | VPtr   of int
(* Élements du tas *)
type heap_value =
  | VClos  of string * expr * value Env.t
  | VStrct of (string, value) Hashtbl.t

let print_value = function
  | VInt n  -> Printf.printf "%d\n" n
  | VBool b -> Printf.printf "%b\n" b
  | VUnit   -> Printf.printf "()\n"
  | VPtr p  -> Printf.printf "@%d\n" p

(* Interprétation d'un programme complet *)
let eval_prog (p: prog): value =
  
  (* Initialisation de la mémoire globale *)
  let (mem: (int, heap_value) Hashtbl.t) = Hashtbl.create 16 in

  (* Création de nouvelles adresses *)
  let new_ptr =
    let cpt = ref 0 in
    fun () -> incr cpt; !cpt
  in 
 

  (* Interprétation d'une expression, en fonction d'un environnement
     et de la mémoire globale *)
  let rec eval (e: expr) (env: value Env.t): value = 
    match e with
    | Int n  -> VInt n
    | Bool b -> VBool b
    | Unit -> VUnit
    | Uop(Not, e) -> VBool (not (evalb e env))
    | Uop(Neg, e) -> VInt (-(evali e env))
    | Bop(Add, e1, e2) -> VInt (evali e1 env + evali e2 env)
    | Bop(Sub, e1, e2) -> VInt (evali e1 env - evali e2 env)
    | Bop(Mul, e1, e2) -> VInt (evali e1 env * evali e2 env)
    | Bop(Div, e1, e2) -> VInt (evali e1 env / evali e2 env)
    | Bop(Mod, e1, e2) -> VInt (evali e1 env mod evali e2 env)
    | Bop(Lt, e1, e2) -> VBool (evali e1 env < evali e2 env)
    | Bop(Le, e1, e2) -> VBool (evali e1 env <= evali e2 env)
    | Bop(Eq, e1, e2) -> begin
      match eval e1 env with
        | VInt n1 -> begin
          match eval e2 env with
            | VInt n2 -> VBool (n1 = n2)
            | _ -> assert false
          end
        | VBool b1 -> begin
          match eval e2 env with
            | VBool b2 -> VBool (b1 = b2)
            | _ -> assert false
          end
        | _ -> assert false
      end
    | Bop(Neq, e1, e2) -> begin
      match eval e1 env with
        | VInt n1 -> begin 
          match eval e2 env with
            | VInt n2 -> VBool (n1 <> n2)
            | _ -> assert false
          end
        | VBool b1 -> begin
          match eval e2 env with
            | VBool b2 -> VBool (b1 <> b2)
            | _ -> assert false
          end
        | _ -> assert false
      end
    | Bop(And, e1, e2) -> VBool (evalb e2 env && evalb e1 env)
    | Bop(Or, e1, e2) -> VBool (evalb e2 env || evalb e1 env)
    | Var(x) -> Env.find x env
    | Let(x, e1, e2) -> let eval1 = eval e1 env in
      eval e2 (Env.add x eval1 env)
    | If(e1, e2, e3) -> if (evalb e1 env) then (eval e2 env) else (eval e3 env)
    | Fun (s, _, e) -> 
      let addr = new_ptr() in
      Hashtbl.add mem addr (VClos(s, e, env)); VPtr addr
    | App (e1, e2) ->
      let addr = evalptr e1 env in
      let eval2 = eval e2 env in
      let v = Hashtbl.find mem addr in begin
      match v with 
        | VClos (x, eval1, env1) -> eval eval1 (Env.add x eval2 env1)
        | _ -> assert false
      end
   
    | Seq(e1, e2) -> let _ = eval e1 env in eval e2 env
    | _ -> VInt 1
    

  (* Évaluation d'une expression dont la valeur est supposée entière *)
  and evali (e: expr) (env: value Env.t): int = 
    match eval e env with
    | VInt n -> n
    | _ -> assert false

  (* Évaluation d'une expression dont la valeur est supposée booléenne *)
  and evalb (e: expr) (env: value Env.t): bool =
    match eval e env with
    | VBool b -> b
    | _ -> assert false

  and evalptr (e: expr) (env: value Env.t): int =
    match eval e env with
    | VPtr p ->
      let x = Hashtbl.find_opt mem p in
      if (is_some(x)) then p else assert false
    | _ -> assert false
  in

  eval p.code Env.empty