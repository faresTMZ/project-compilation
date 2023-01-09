open Mml
open Option

(* Environnement de typage : associe des types aux noms de variables *)
module SymTbl = Map.Make(String)
type tenv = typ SymTbl.t

(* Pour remonter des erreurs circonstanciées *)
exception Type_error of string

let error s = raise (Type_error s)

let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s but got %s" 
           (typ_to_string ty_expected) (typ_to_string ty_actual))

let expected_tfun =
  error (Printf.sprintf "expected TFun")

  let redundance_type_error typ_name =
  error (Printf.sprintf "The type %s is already declared." typ_name)

let uncorrect_type_error = 
  error (Printf.sprintf "The structure assignation doesn't correspond to any custom types declaration.")

let not_mutable_type_error field =
  error (Printf.sprintf "The field %s isn't mutable." field)

let expected_tstruct =
  error (Printf.sprintf "expected TStrct") 

(* Vérification des types d'un programme *)
let type_prog prog =

  (* Fonction intermédiaire pour initialiser les types des champs des structures *)
  let rec type_strct s senv = match s with
    | [] -> senv
    | (field, typ, mut) :: t -> type_strct t (SymTbl.add field (typ, mut) senv)
  in
  (* Initialise l'environnement avec les types des structures *)
  let rec types_init t tenv = match t with
    | [] -> tenv
    | (type_name, structure) :: tail -> 
      if (is_some(SymTbl.find_opt type_name tenv)) then
      redundance_type_error type_name else
      types_init tail (SymTbl.add type_name (type_strct structure SymTbl.empty) tenv)
  in
  let type_map = types_init prog.types SymTbl.empty
  in
  
  (* Vérifie que tous les champs d'une structure correspondent a un type de la type_map *)
  let rec check_bool e t tenv =
    let typ_e = type_expr e tenv in
    typ_e = t

  and check_strct_bis l t tenv =
    (* on compare chaque élément de l *)
    match l with
      | [] -> true
      | (x, e) :: tail ->
        let bndng = SymTbl.find_opt x t in
        if (is_some(bndng)) then begin
          let (z, _) = get bndng in 
          if (check_bool e z tenv) then
          check_strct_bis tail t tenv else
          false
        end else false

  and check_strct_types l tenv =
    SymTbl.filter (fun _ map ->
        (* on compare les deux taille *)
        if (List.length l <> SymTbl.cardinal map) then false else
        check_strct_bis l map tenv
    ) type_map

  (* Vérifie que l'expression [e] a le type [typ] *)
  and check e typ tenv =
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
        | _ -> expected_tfun
      end
    | Seq (e1, e2) ->
      let t1 = type_expr e1 tenv in
      if t1 <> TUnit then (Printf.printf "expected type Unit but got %s"  (typ_to_string t1));
      type_expr e2 tenv
    | Fix (x, t1, e) -> type_expr e (SymTbl.add x t1 tenv)
    | Strct s ->
      let m = check_strct_types s tenv in
      if (SymTbl.cardinal m <> 1) then
      uncorrect_type_error else
      let (key, _) = (SymTbl.choose m) in TStrct key
    | GetF (e, x) -> begin
      match type_expr e tenv with
        | TStrct s ->
          let (t, _) = SymTbl.find x (SymTbl.find s type_map) in t
        | _ -> expected_tstruct
      end
    | SetF (e, x, _) -> begin
      match type_expr e tenv with
        | TStrct s ->
          let (_, mut) = SymTbl.find x (SymTbl.find s type_map) in
          if mut then TUnit else not_mutable_type_error x 
        | _ -> expected_tstruct
      end

  in
  type_expr prog.code SymTbl.empty