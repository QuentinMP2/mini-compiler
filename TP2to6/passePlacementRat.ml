(* AstType.instruction -> int -> string -> AstPlacement.instruction * int *)
(* instruction, depl, reg -> instruction * taille *)
let rec analyse_placement_instruction i depl reg =
  match i with
  | AstType.Declaration (info,e) -> begin
      (* récupérer le type t dans l'info i *)
      let t, n, _, _ = !info in
      (* ajouter depl[reg] dans l'info i *)
      let new_info = t, n, depl, reg in
      (AstPlacement.Declaration(ref new_info, e), getTaille t)
  end
  | AstType.Conditionnelle (c,t,e) ->
      (**)
      let nt = analyse_placement_bloc t depl reg in
      let ne = analyse_placement_bloc e depl reg in
      (AstPlacement.Conditionnelle(c, nt, ne), 0)
  | AstType.TantQue (c,b) ->
      (* comme au dessus *)
  | AstType.Retour (e, ia) ->
      let _, t, ltp = !ia in
      let tp = List.fold_left (fun x acc -> getTaille x + acc) 0 ltp in 
      (AstPlacement.Retour(e, getTaille t, depl), 0)
  | AstType.Affectation (ia,e) ->
      (AstPlacement.Affectation(i,e), 0)
  | AstType.AffichageInt e ->
      (AstPlacement.AffichageInt(e), 0)
  | AstType.AffichageRat e ->
      (AstPlacement.AffichageRat(e), 0)
  | AstType.AffichageBool e ->
      (AstPlacement.AffichageBool(e), 0)
  | AstType.Empty ->
      (AstPlacement.Empty, 0)
and
(* analyse_placement_bloc : AstType.bloc -> int -> string -> AstPlacement.bloc *)
analyse_placement_bloc li depl reg =
  let nli, n = List.fold_left (fun x acc -> 
      let liacc, tacc = acc in
      let i, t = analyser_placement_instruction x tacc reg in 
      liacc@[i], tacc + t
  ) ([], 0) li in
  AstPlacement.bloc(nli, n)
  
  (* OU *)
  match li with
  | [] -> ([],0)
  | i::q -> 
      let (ni,ti) = analyse_placement_instruction i dep reg in
      let (nli,tb) = analyse_placement_bloc q (ti+dep) reg in
      (ni::nli, ti+tb)

(* AstType.programme -> AstPlacement.programme *)
let analyser (AstType.Programme (fonctions,prog)) =
  let nf = List.map analyse_placement_fonction fonctions in
  let nb = analyse_placement_bloc prog 0 "SB" in
  AstPlacement.Programme(nf,nb)


let analyse_placement_fonction (AstType.Fonction(info, lp, li)) = 
  (* analyser les paramètres : TODO *)
  let rec analyse_placement_param lp dep =
      match lp with
      | [] -> 0
      | i::q -> 
          let tailleq = analyse_placement_param q in
          let taillei = taille du type dans i in
          modifier adresse de i à -(tailleq+taillei)[LB]
  
  let nb = analyse_deplacement_bloc li 3 "LB"
  AstPlacement.Fonction(i, lp, nb)
