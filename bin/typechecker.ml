open Mml

(* Environnement de typage : associe des types aux noms de variables *)
module SymTbl = Map.Make(String)
type tenv = typ SymTbl.t

(* Pour remonter des erreurs circonstanciées *)
exception Type_error of string
let error s = raise (Type_error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s but got %s" 
           (typ_to_string ty_expected) (typ_to_string ty_actual))

(* Vérification des types d'un programme *)
let type_prog prog =

  (* Vérifie que l'expression [e] a le type [typ] *)
  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
      if typ_e <> typ then type_error typ_e typ

  (* Calcule le type de l'expression [e] *)
  and type_expr e tenv = match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unit -> TUnit
    | Uop (Neg, e) -> check e TInt tenv; TInt
    | Uop (Not, e) -> check e TBool tenv; TBool
    | Bop((Add | Mul | Sub | Div | Mod), e1, e2) ->
       check e1 TInt tenv; check e2 TInt tenv; TInt
    | Bop ((Eq | Neq), e1, e2) -> let typ1 = type_expr e1 tenv in check e2 typ1 tenv; TBool
    | Bop ((Lt | Le), e1 , e2) -> check e1 TInt tenv; check e2 TInt tenv; TBool
    | Bop ((And | Or), e1 , e2) -> check e1 TBool tenv; check e2 TBool tenv; TBool
    | Var x -> SymTbl.find x tenv
    | Let (x, e1, e2) ->
      let t1 = type_expr e1 tenv in
      let t2 = type_expr e2 (SymTbl.add x t1 tenv) in t2
    | If (e0, e1, e2) ->
      let t1 = type_expr e1 tenv in check e2 t1 tenv;
      check e0 TBool tenv; t1
    | Fun (x, t1, e) -> let t2 = type_expr e (SymTbl.add x t1 tenv) in TFun(t1, t2)
    | App (e1, e2) -> begin
      match type_expr e1 tenv with
      | TFun(t2, t1) -> 
        let typ_e2 = type_expr e2 tenv in
        if t2 = typ_e2 then t1 else type_error t2 typ_e2
      | _ -> error (Printf.sprintf "expected TFun") 
    end
    | Seq (e1, e2) ->
      let t1 = type_expr e1 tenv in
      if t1 <> TUnit then (Printf.printf "expected type Unit but got %s"  (typ_to_string t1));
      type_expr e2 tenv
    | Fix (x, t1, e) -> type_expr e (SymTbl.add x t1 tenv)
    (* | Strct s ->   ; TStrct s
    | GetF (e, x) -> check e TStrct(e) tenv; type_expr e.x tenv
    | SetF (e1, x, e2) -> check e1 TStrct tenv; *)
    | _ -> TInt

  in
  type_expr prog.code SymTbl.empty
