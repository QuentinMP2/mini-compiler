open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

(* analyse_placement_instruction : AstType.instruction -> int -> string -> AstPlacement.instruction * int *)
(* Paramètre : l'instruction à analyser *)
(* Paramètre : le déplacement relatif au début du registre mémoire *)
(* Paramètre : le registre mémoire dans lequel enregistrer *)
(* Ajoute le placement mémoire dans les infos de l'AST dans les instructions *)
let rec analyse_placement_instruction i depl reg =
  match i with
  | AstType.Declaration (info, e) -> begin
    (* récupérer le type t dans l'info i *)
    match !info with
    | InfoVar (_, t, _, _) ->
      (* ajouter depl[reg] dans l'info i *)
      modifier_adresse_variable depl reg info;
      (AstPlacement.Declaration (info, e), getTaille t)
    | _ -> failwith "erreur interne"
  end
  | AstType.Conditionnelle (c, t, e) ->
    (* On analyse les blocs t et e *)
    let nt = analyse_placement_bloc t depl reg in
    let ne = analyse_placement_bloc e depl reg in
    (AstPlacement.Conditionnelle (c, nt, ne), 0)
  | AstType.TantQue (c, b) ->
    (* On analyse le bloc b *)
    let nb = analyse_placement_bloc b depl reg in
    (AstPlacement.TantQue (c, nb), 0)
  | AstType.Retour (e, ia) -> begin
    match !ia with
    | InfoFun (_, t, ltp) ->
      (* On récupere la taille totale des paramètres de la fonction lié à l'instuction retour *)
      let tp = List.fold_left (fun acc x -> getTaille x + acc) 0 ltp in
      (AstPlacement.Retour (e, getTaille t, tp), 0)
    | _ -> failwith "erreur interne"
  end
  | AstType.Affectation (a, e) -> (AstPlacement.Affectation (a, e), 0)
  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)
  | AstType.Empty -> (AstPlacement.Empty, 0)

(* analyse_placement_bloc : AstType.bloc -> int -> string -> AstPlacement.bloc *)
(* Paramètre : le bloc (liste d'instruction) à analyser *)
(* Paramètre : le déplacement relatif au début du registre mémoire *)
(* Paramètre : le registre mémoire dans lequel enregistrer *)
(* Ajoute le placement mémoire dans les infos de l'AST dans les blocs *)
and analyse_placement_bloc li depl reg =
  match li with
  (* Liste d'instruction vide -> on revoie une liste vide et un deplacement de 0 *)
  | [] -> ([], 0)
  | i :: q ->
    (* On analyse l'instruction courrante *)
    let ni, ti = analyse_placement_instruction i depl reg in
    (* on analyses la suite des expressions *)
    let nli, tb = analyse_placement_bloc q (ti + depl) reg in
    (ni :: nli, ti + tb)

(* analyse_placement_fonction : AstType.fonction -> AstPlacement.fonction *)
(* Paramètre : la fonction à analyser à analyser *)
(* Ajoute le placement mémoire dans les infos de l'AST dans les fonctions *)
let analyse_placement_fonction (AstType.Fonction (info, lp, li)) =
  (* info_ast list -> int *)
  (* Paramètre : liste des paramètres *)
  (* fonction auxiliaire qui analyse les paramètres de la fonction *)
  let rec analyse_placement_param slp =
    match slp with
    | [] -> 0
    | i :: q -> (
      (* On récupère la taille des paramètres suivant celui qu'on considère *)
      let tailleq = analyse_placement_param q in
      match !i with
      | InfoVar (_, t, _, _) ->
        (* On récupère la taille du type du paramètre *)
        let taillei = getTaille t in
        (* On modifie son adresse par effet de bord *)
        modifier_adresse_variable (-(taillei + tailleq)) "LB" i;
        tailleq + taillei
      | _ -> failwith "erreur interne")
  in
  let _ = analyse_placement_param lp in
  let nb = analyse_placement_bloc li 3 "LB" in
  AstPlacement.Fonction (info, lp, nb)

(* analyser : AstType.programme -> AstPlacement.programme *)
(* Paramètre : le programme à analyser *)
(* Ajoute le placement mémoire dans les infos de l'AST dans le programme *)
let analyser (AstType.Programme (fonctions, prog)) =
  let nf = List.map analyse_placement_fonction fonctions in
  let nb = analyse_placement_bloc prog 0 "SB" in
  AstPlacement.Programme (nf, nb)
