open Tds
open Ast
open Type
open Tam
open Code

type t1 = Ast.AstPlacement.programme
type t2 = string

(* AstPlacement.affectable -> bool -> string -> int -> string *)
(* paramètre a : l'affectable à analyser *)
(* paramètre en_ecriture : on utilise l'affectable dans le cadre d'une ecriture ou d'une lecture *)
(* paramètre acc : accumulateur de loadi *)
(* paramètre prof : profondeur des appels en déréférencement *)
let rec analyse_code_affectable (a : AstPlacement.affectable) en_ecriture acc
    prof =
  match a with
  | Ident info -> begin
    match !info with
    | InfoVar (_, t, depl, reg) ->
      let n = getTaille (type_prof t prof) in
      loada depl reg ^ acc ^ if en_ecriture then storei n else loadi n
    | _ -> failwith "erreur interne : code_affectable Ident"
  end
  | Deref aff ->
    analyse_code_affectable aff en_ecriture (acc ^ loadi 1) (prof + 1)

(* AstPlacement.expression -> string *)
let rec analyse_code_expression (e : AstPlacement.expression) =
  match e with
  | AppelFonction (i, le) -> begin
    List.fold_left (fun acc x -> acc ^ analyse_code_expression x) "" le
    ^
    match !i with
    | InfoFun (nom, _, _) -> call "SB" nom
    | _ -> failwith "erreur interne : code_expression AppelFonction"
  end
  | Affectable a -> analyse_code_affectable a false "" 0
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
  | Adresse info -> begin
    match !info with
    | InfoVar (_, _, depl, reg) -> loada depl reg
    | _ -> failwith "erreur interne : code_expression Adresse"
  end
  | New t -> loadl_int (getTaille t) ^ subr "MAlloc"
  | Null -> ""

(* AstPlacement.instruction -> bool -> string *)
(* Paramètre est_bloc_VG : traitement different pour les variables gobales *)
let rec analyse_code_instruction (i : AstPlacement.instruction) est_bloc_VG =
  match i with
  | Declaration (info, e) -> begin
    match !info with
    | InfoVar (_, t, depl, reg) ->
      (* On push seulement si on est pas dans le bloc des variables globales auquels cas c'est déjà fait *)
      let psh = if est_bloc_VG then "" else push (getTaille t) in
      psh ^ analyse_code_expression e ^ store (getTaille t) depl reg
    | _ -> failwith "erreur interne : code_instruction Declaration"
  end
  | Affectation (a, e) ->
    analyse_code_expression e ^ analyse_code_affectable a true "" 0
  | AffichageInt e -> analyse_code_expression e ^ subr "IOut"
  | AffichageRat e -> analyse_code_expression e ^ call "SB" "ROut"
  | AffichageBool e -> analyse_code_expression e ^ subr "BOut"
  | Conditionnelle (e, bt, be) ->
    let etiquetteE = label (getEtiquette ()) in
    let etiquetteFin = label (getEtiquette ()) in
    analyse_code_expression e ^ jumpif 0 etiquetteE ^ analyse_code_bloc bt false
    ^ jump etiquetteFin ^ etiquetteE ^ analyse_code_bloc be false ^ etiquetteFin
  | TantQue (e, bloc) ->
    let etiquetteE = label (getEtiquette ()) in
    let etiquetteFin = label (getEtiquette ()) in
    etiquetteE ^ analyse_code_expression e ^ jumpif 0 etiquetteFin
    ^ analyse_code_bloc bloc false
    ^ jump etiquetteE ^ etiquetteFin
  | Retour (e, tailleRet, tailleParam) ->
    analyse_code_expression e ^ return tailleRet tailleParam
  | Empty -> "\n"
  | StatiqueL (info, e) -> begin
    match !info with
    | InfoVar (_, t, depl, reg) ->
      let etiquetteIF = label (getEtiquette ()) in
      (* On teste si la variable statique est déjà initialisé *)
      load (getTaille Bool) (depl + getTaille t) reg
      (* Si c'est le cas on saute cette étape *)
      ^ jumpif 1 etiquetteIF
      (* Sinon on l'initialise *)
      ^ push (getTaille t)
      ^ analyse_code_expression e
      ^ store (getTaille t) depl reg
      (* Et on met un booleen à vrai pour notifier l'initialisation *)
      ^ push (getTaille Bool)
      ^ loadl_int 1
      ^ store (getTaille Bool) (depl + getTaille t) reg
      ^ etiquetteIF
    | _ -> failwith "erreur interne : code_instruction Declaration"
  end

(* AstPlacement.bloc -> bool -> string *)
(* Paramètre est_bloc_VG : traitement différent pour les variables gobales *)
and analyse_code_bloc (li, taille) est_bloc_VG =
  List.fold_left
    (fun acc i -> acc ^ analyse_code_instruction i est_bloc_VG)
    "" li
  ^ pop 0 taille

(* AstPlacement.fonction -> string *)
let analyse_code_fonction (AstPlacement.Fonction (info, _, (li, taille))) =
  let labelF =
    match !info with
    | InfoFun (nom, _, _) -> nom
    | _ -> failwith "erreur interne : code_fonction"
  in
  let nli = analyse_code_bloc (li, taille) false in
  label labelF ^ nli ^ halt

(* AstPlacement.programme -> string *)
let analyser (AstPlacement.Programme ((lvg, empl_main), fonctions, prog)) =
  (* Permet d'éviter les pop inutiles en debut de code *)
  let slvg = if lvg = [] then "" else (analyse_code_bloc (lvg, 0)) true in
  push empl_main ^ slvg ^ getEntete ()
  ^ List.fold_left (fun acc x -> acc ^ analyse_code_fonction x) "" fonctions
  ^ label "main"
  ^ analyse_code_bloc prog false
  ^ pop 0 empl_main ^ halt
