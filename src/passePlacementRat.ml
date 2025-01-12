open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

(* analyse_placement_instruction : AstType.instruction -> int -> string -> int -> AstPlacement.instruction * int * int *)
(* Paramètre : l'instruction à analyser *)
(* Paramètre : le déplacement relatif au début du registre mémoire *)
(* Paramètre : le registre mémoire dans lequel enregistrer *)
(* Paramètre : emplacement où stocker la prochaine variable statique locale *)
(* Ajoute le placement mémoire dans les infos de l'AST dans les instructions *)
let rec analyse_placement_instruction i depl reg emplSL =
  match i with
  | AstType.Declaration (info, e) -> begin
    (* récupérer le type t dans l'info i *)
    match !info with
    | InfoVar (_, t, _, _) ->
      (* ajouter depl[reg] dans l'info i *)
      modifier_adresse_variable depl reg info;
      (AstPlacement.Declaration (info, e), getTaille t, emplSL)
    | _ -> failwith "erreur interne : placement_instruction - declaration"
  end
  | AstType.Conditionnelle (c, t, e) ->
    (* On analyse les blocs t et e *)
    (* n? pour nouveau bloc, t? pour taille du bloc *)
    let nt, tt, deplSt = analyse_placement_bloc t depl reg emplSL in
    let ne, te, deplSe = analyse_placement_bloc e depl reg deplSt in
    (AstPlacement.Conditionnelle (c, (nt, tt), (ne, te)), 0, deplSe)
  | AstType.TantQue (c, b) ->
    (* On analyse le bloc b *)
    let nb, tb, deplSb = analyse_placement_bloc b depl reg emplSL in
    (AstPlacement.TantQue (c, (nb, tb)), 0, deplSb)
  | AstType.Retour (e, ia) -> begin
    match !ia with
    | InfoFun (_, t, ltp) ->
      (* On récupere la taille totale des paramètres de la fonction lié à l'instuction retour *)
      let tp = List.fold_left (fun acc x -> getTaille x + acc) 0 ltp in
      (AstPlacement.Retour (e, getTaille t, tp), 0, emplSL)
    | _ -> failwith "erreur interne : placement_instruction - retour"
  end
  | AstType.Affectation (a, e) -> (AstPlacement.Affectation (a, e), 0, emplSL)
  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0, emplSL)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0, emplSL)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0, emplSL)
  | AstType.Empty -> (AstPlacement.Empty, 0, emplSL)
  | AstType.StatiqueL (info, e) -> begin
    (* récupérer le type t dans l'info i *)
    match !info with
    | InfoVar (_, t, _, _) ->
      (* ajouter depl[reg] dans l'info i *)
      modifier_adresse_variable emplSL "SB" info;
      ( AstPlacement.StatiqueL (info, e),
        0,
        emplSL + getTaille t + getTaille Bool )
      (* car on veut garder un booléen qui indique si la variable statique locale a été initialisée ou non *)
    | _ -> failwith "erreur interne : placement_instruction - declaration"
  end

(* analyse_placement_bloc : AstType.bloc -> int -> string -> int -> AstPlacement.bloc * int * int *)
(* Paramètre : le bloc (liste d'instruction) à analyser *)
(* Paramètre : le déplacement relatif au début du registre mémoire *)
(* Paramètre : le registre mémoire dans lequel enregistrer *)
(* Paramètre : emplacement où stocker la prochaine variable statique locale *)
(* Ajoute le placement mémoire dans les infos de l'AST dans les blocs *)
(* Renvoie aussi la taille du bloc et la taille des variables statiques locales *)
and analyse_placement_bloc li depl reg emplSL =
  match li with
  (* Liste d'instruction vide -> on revoie une liste vide et un deplacement de 0 *)
  | [] -> ([], 0, emplSL)
  | i :: q ->
    (* On analyse l'instruction courrante *)
    let ni, ti, tsl = analyse_placement_instruction i depl reg emplSL in
    (* on analyses la suite des expressions *)
    let nli, tb, ndeplS = analyse_placement_bloc q (ti + depl) reg tsl in
    (ni :: nli, ti + tb, ndeplS)

(* analyse_placement_fonction : int -> AstType.fonction -> (AstPlacement.fonction * int) *)
(* Paramètre : emplacement où stocker la prochaine variable statique locale *)
(* Paramètre : la fonction à analyser *)
(* Ajoute le placement mémoire dans les infos de l'AST dans les fonctions *)
let analyse_placement_fonction emplSL (AstType.Fonction (info, lp, li)) =
  (* info_ast list -> int *)
  (* Paramètre : liste des paramètres *)
  (* fonction auxiliaire qui analyse les paramètres de la fonction *)
  let rec analyse_placement_param slp =
    match slp with
    | [] -> 0
    | (i, _) :: q -> (
      (* On récupère la taille des paramètres suivant celui qu'on considère *)
      let tailleq = analyse_placement_param q in
      match !i with
      | InfoVar (_, t, _, _) ->
        (* On récupère la taille du type du paramètre *)
        let taillei = getTaille t in
        (* On modifie son adresse par effet de bord *)
        modifier_adresse_variable (-(taillei + tailleq)) "LB" i;
        tailleq + taillei
      | _ -> failwith "erreur interne : placement_param"
    )
  in
  let _ = analyse_placement_param lp in
  let nb, tb, new_depl_sl = analyse_placement_bloc li 3 "LB" emplSL in
  let nlp = List.map fst lp in
  (AstPlacement.Fonction (info, nlp, (nb, tb)), new_depl_sl)

(* analyser : AstType.programme -> AstPlacement.programme *)
(* Paramètre : le programme à analyser *)
(* Ajoute le placement mémoire dans les infos de l'AST dans le programme *)
let analyser (AstType.Programme (variablesG, fonctions, prog)) =
  let lvg, depl, _ = analyse_placement_bloc variablesG 0 "SB" 0 in
  let nf, ndepl =
    List.fold_left
      (fun acc f ->
        let lf, depl_acc_sl = acc in
        let nf, new_depl_sl = analyse_placement_fonction depl_acc_sl f in
        (nf :: lf, new_depl_sl))
      ([], depl) fonctions
  in
  let nb, tb, _ = analyse_placement_bloc prog ndepl "SB" 0 in
  AstPlacement.Programme ((lvg, ndepl), nf, (nb, tb))
