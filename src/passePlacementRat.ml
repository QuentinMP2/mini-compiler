open Tds
open Ast
open Type

type t1 = Ast.AstType.programme
type t2 = Ast.AstPlacement.programme

(* AstType.instruction -> int -> string -> AstPlacement.instruction * int *)
(* instruction, depl, reg -> instruction * taille *)
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
    let nt = analyse_placement_bloc t depl reg in
    let ne = analyse_placement_bloc e depl reg in
    (AstPlacement.Conditionnelle (c, nt, ne), 0)
  | AstType.TantQue (c, b) ->
    let nb = analyse_placement_bloc b depl reg in
    (AstPlacement.TantQue (c, nb), 0)
  | AstType.Retour (e, ia) -> begin
    match !ia with
    | InfoFun (_, t, ltp) ->
      let tp = List.fold_left (fun acc x -> getTaille x + acc) 0 ltp in
      (AstPlacement.Retour (e, getTaille t, tp), 0)
    | _ -> failwith "erreur interne"
  end
  | AstType.Affectation (ia, e) -> (AstPlacement.Affectation (ia, e), 0)
  | AstType.AffichageInt e -> (AstPlacement.AffichageInt e, 0)
  | AstType.AffichageRat e -> (AstPlacement.AffichageRat e, 0)
  | AstType.AffichageBool e -> (AstPlacement.AffichageBool e, 0)
  | AstType.Empty -> (AstPlacement.Empty, 0)

and analyse_placement_bloc li depl reg =
  (* analyse_placement_bloc : AstType.bloc -> int -> string -> AstPlacement.bloc *)
  (* let nli, n = List.fold_left (fun x acc ->
         let liacc, tacc = acc in
         let i, t = analyser_placement_instruction x tacc reg in
         liacc@[i], tacc + t
     ) ([], 0) li in
     AstPlacement.bloc(nli, n) *)

  (* OU *)
  match li with
  | [] -> ([], 0)
  | i :: q ->
    let ni, ti = analyse_placement_instruction i depl reg in
    let nli, tb = analyse_placement_bloc q (ti + depl) reg in
    (ni :: nli, ti + tb)

let analyse_placement_fonction (AstType.Fonction (info, lp, li)) =
  (* analyser les paramètres *)
  let rec analyse_placement_param slp =
    match slp with
    | [] -> 0
    | i :: q -> (
      let tailleq = analyse_placement_param q in
      match !i with
      | InfoVar (_, t, _, _) ->
        let taillei = getTaille t in
        modifier_adresse_variable (-(taillei + tailleq)) "LB" i;
        tailleq + taillei
      | _ -> failwith "erreur interne")
  in
  let _ = analyse_placement_param lp in
  let nb = analyse_placement_bloc li 3 "LB" in
  AstPlacement.Fonction (info, lp, nb)

(* AstType.programme -> AstPlacement.programme *)
let analyser (AstType.Programme (fonctions, prog)) =
  let nf = List.map analyse_placement_fonction fonctions in
  let nb = analyse_placement_bloc prog 0 "SB" in
  AstPlacement.Programme (nf, nb)
