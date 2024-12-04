open Tds
open Ast
open Type
open Tam
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string

(* AstPlacement.expression -> string *)
let rec analyse_code_expression (e : AstPlacement.expression) =
  match e with
  | AppelFonction (i, le) -> begin
    List.fold_left (fun acc x -> acc ^ analyse_code_expression x) "" le
    ^
    match !i with
    | InfoFun (nom, _, _) -> call "SB" nom
    | _ -> failwith "erreur interne"
  end
  | Ident info -> begin
    match !info with
    | InfoVar (_, t, depl, reg) -> load (getTaille t) depl reg
    | _ -> failwith "erreur interne"
  end
  | Booleen b -> loadl_int (Bool.to_int b)
  | Entier n -> loadl_int n
  | Unaire (op, exp) -> begin
    analyse_code_expression exp
    ^ match op with Numerateur -> pop 0 1 | Denominateur -> pop 1 1
  end
  | Binaire (op, e1, e2) -> begin
    analyse_code_expression e1 ^ analyse_code_expression e2
    ^
    match op with
    | Fraction -> call "SB" "norm"
    | PlusInt -> subr "IAdd"
    | PlusRat -> call "SB" "RAdd"
    | MultInt -> subr "IMul"
    | MultRat -> call "SB" "RMul"
    | EquInt -> subr "IEq"
    | EquBool -> subr "IEq"
    | Inf -> subr "ILss"
  end

(* AstPlacement.instruction -> string *)
let rec analyse_code_instruction (i : AstPlacement.instruction) =
  match i with
  | Declaration (info, e) -> begin
    match !info with
    | InfoVar (_, t, depl, reg) ->
      push (getTaille t)
      ^ analyse_code_expression e
      ^ store (getTaille t) depl reg
    | _ -> failwith "erreur interne"
  end
  | Affectation (info, e) -> begin
    match !info with
    | InfoVar (_, t, depl, reg) ->
      analyse_code_expression e ^ store (getTaille t) depl reg
    | _ -> failwith "erreur interne"
  end
  | AffichageInt e -> analyse_code_expression e ^ subr "IOut"
  | AffichageRat e -> analyse_code_expression e ^ call "SB" "ROut"
  | AffichageBool e -> analyse_code_expression e ^ subr "BOut"
  | Conditionnelle (e, bt, be) ->
    let etiquetteE = label (getEtiquette ()) in
    let etiquetteFin = label (getEtiquette ()) in
    analyse_code_expression e ^ jumpif 0 etiquetteE ^ analyse_code_bloc bt
    ^ jump etiquetteFin ^ etiquetteE ^ analyse_code_bloc be ^ etiquetteFin
  | TantQue (e, bloc) ->
    let etiquetteE = label (getEtiquette ()) in
    let etiquetteFin = label (getEtiquette ()) in
    etiquetteE ^ analyse_code_expression e ^ jumpif 0 etiquetteFin
    ^ analyse_code_bloc bloc ^ jump etiquetteE ^ etiquetteFin
  | Retour (e, tailleRet, tailleParam) ->
    analyse_code_expression e ^ return tailleRet tailleParam
  | Empty -> "\n"

(* AstPlacement.bloc -> string *)
and analyse_code_bloc (li, taille) =
  List.fold_left (fun acc i -> acc ^ analyse_code_instruction i) "" li
  ^ pop 0 taille

(* AstPlacement.fonction -> string *)
let analyse_code_fonction (AstPlacement.Fonction (info, _, (li, taille))) =
  let labelF =
    match !info with
    | InfoFun (nom, _, _) -> nom
    | _ -> failwith "erreur interne"
  in
  let nli = analyse_code_bloc (li, taille) in
  label labelF ^ nli ^ halt

(* AstPlacement.programme -> string *)
let analyser (AstPlacement.Programme (fonctions, prog)) =
  getEntete ()
  ^ List.fold_left (fun acc x -> acc ^ analyse_code_fonction x) "" fonctions
  ^ "main\n" ^ analyse_code_bloc prog ^ halt
