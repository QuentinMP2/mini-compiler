(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme

(* analyse_tds_expression : tds -> AstSyntax.expression -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
   en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)

let rec analyse_tds_expression tds e =
  match e with
  | AstSyntax.AppelFonction (id, le) -> begin
    match chercherGlobalement tds id with
    | None -> raise (IdentifiantNonDeclare id)
    | Some info -> (
      match !info with
      | InfoFun _ ->
        AstTds.AppelFonction (info, List.map (analyse_tds_expression tds) le)
      | _ -> raise (MauvaiseUtilisationIdentifiant id))
  end
  | AstSyntax.Ident n -> begin
    match chercherGlobalement tds n with
    | None -> raise (IdentifiantNonDeclare n)
    | Some info -> begin
      match !info with
      | InfoVar _ -> AstTds.Ident info
      | InfoConst (_, value) -> AstTds.Entier value
      | _ -> raise (MauvaiseUtilisationIdentifiant n)
    end
  end
  | AstSyntax.Binaire (b, e1, e2) ->
    AstTds.Binaire
      (b, analyse_tds_expression tds e1, analyse_tds_expression tds e2)
  | AstSyntax.Unaire (op, e1) ->
    AstTds.Unaire (op, analyse_tds_expression tds e1)
  | AstSyntax.Booleen b -> AstTds.Booleen b
  | AstSyntax.Entier i -> AstTds.Entier i

(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
   en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds oia i =
  match i with
  | AstSyntax.Declaration (t, n, e) -> begin
    match chercherLocalement tds n with
    | None ->
      (* L'identifiant n'est pas trouvé dans la tds locale,
         il n'a donc pas été déclaré dans le bloc courant *)
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Création de l'information associée à l'identfiant *)
      let info = InfoVar (n, Undefined, 0, "") in
      (* Création du pointeur sur l'information *)
      let ia = ref info in
      (* Ajout de l'information (pointeur) dans la tds *)
      ajouter tds n ia;
      (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
         et l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Declaration (t, ia, ne)
    | Some _ ->
      (* L'identifiant est trouvé dans la tds locale,
         il a donc déjà été déclaré dans le bloc courant *)
      raise (DoubleDeclaration n)
  end
  | AstSyntax.Affectation (n, e) -> begin
    match chercherGlobalement tds n with
    | None ->
      (* L'identifiant n'est pas trouvé dans la tds globale. *)
      raise (IdentifiantNonDeclare n)
    | Some info -> (
      (* L'identifiant est trouvé dans la tds globale,
         il a donc déjà été déclaré. L'information associée est récupérée. *)
      match !info with
      | InfoVar _ ->
        (* Vérification de la bonne utilisation des identifiants dans l'expression *)
        (* et obtention de l'expression transformée *)
        let ne = analyse_tds_expression tds e in
        (* Renvoie de la nouvelle affectation où le nom a été remplacé par l'information
           et l'expression remplacée par l'expression issue de l'analyse *)
        AstTds.Affectation (info, ne)
      | _ ->
        (* Modification d'une constante ou d'une fonction *)
        raise (MauvaiseUtilisationIdentifiant n))
  end
  | AstSyntax.Constante (n, v) -> begin
    match chercherLocalement tds n with
    | None ->
      (* L'identifiant n'est pas trouvé dans la tds locale,
         il n'a donc pas été déclaré dans le bloc courant *)
      (* Ajout dans la tds de la constante *)
      ajouter tds n (ref (InfoConst (n, v)));
      (* Suppression du noeud de déclaration des constantes devenu inutile *)
      AstTds.Empty
    | Some _ ->
      (* L'identifiant est trouvé dans la tds locale,
         il a donc déjà été déclaré dans le bloc courant *)
      raise (DoubleDeclaration n)
  end
  | AstSyntax.Affichage e ->
    (* Vérification de la bonne utilisation des identifiants dans l'expression *)
    (* et obtention de l'expression transformée *)
    let ne = analyse_tds_expression tds e in
    (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
    AstTds.Affichage ne
  | AstSyntax.Conditionnelle (c, t, e) ->
    (* Analyse de la condition *)
    let nc = analyse_tds_expression tds c in
    (* Analyse du bloc then *)
    let tast = analyse_tds_bloc tds oia t in
    (* Analyse du bloc else *)
    let east = analyse_tds_bloc tds oia e in
    (* Renvoie la nouvelle structure de la conditionnelle *)
    AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c, b) ->
    (* Analyse de la condition *)
    let nc = analyse_tds_expression tds c in
    (* Analyse du bloc *)
    let bast = analyse_tds_bloc tds oia b in
    (* Renvoie la nouvelle structure de la boucle *)
    AstTds.TantQue (nc, bast)
  | AstSyntax.Retour e -> begin
    (* On récupère l'information associée à la fonction à laquelle le return est associée *)
    match oia with
    (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
    | None ->
      raise RetourDansMain
      (* Il y a une information -> l'instruction est dans une fonction *)
    | Some ia ->
      (* Analyse de l'expression *)
      let ne = analyse_tds_expression tds e in
      AstTds.Retour (ne, ia)
  end

(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds oia li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
     pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
  let nli = List.map (analyse_tds_instruction tdsbloc oia) li in
  (* afficher_locale tdsbloc ; *)
  (* décommenter pour afficher la table locale *)
  nli

(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
   en une fonction de type AstTds.fonction *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyse_tds_fonction maintds (AstSyntax.Fonction (t, n, lp, li)) =
  let aux_param tds (tt, nn) =
    match chercherLocalement tds nn with
    | Some _ -> raise (DoubleDeclaration nn)
    | None ->
      let infoparam = InfoVar (nn, tt, 0, "") in
      let ria = ref infoparam in
      ajouter tds nn ria;
      (tt, ria)
  in
  let tds_fonction = creerTDSFille maintds in
  match chercherGlobalement maintds n with
  | Some _ -> raise (DoubleDeclaration n)
  | None ->
    let listT =
      List.map
        (fun x ->
          let t, _ = x in
          t)
        lp
    in
    let info = InfoFun (n, t, listT) in
    let ia = ref info in
    ajouter maintds n ia;
    let liste_args = List.map (aux_param tds_fonction) lp in
    let ast_inst = analyse_tds_bloc tds_fonction (Some ia) li in
    AstTds.Fonction (t, ia, liste_args, ast_inst)

(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
   en un programme de type AstTds.programme *)
(* Erreur si mauvaise utilisation des identifiants *)
let analyser (AstSyntax.Programme (fonctions, prog)) =
  let tds = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds) fonctions in
  let nb = analyse_tds_bloc tds None prog in
  AstTds.Programme (nf, nb)
