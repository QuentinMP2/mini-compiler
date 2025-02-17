open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

(* analyse_type_affectable : AstTds.affectable -> AstType.affectable * typ *)
(* Paramètre a : l'affectable à analyser *)
(* Place les types dans les info_ast de l'AST et associe les bonnes
   opérations en fonction des types dans les affectables *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_affectable a =
  match a with
  | AstTds.Ident info -> begin
    match !info with
    | InfoVar (_, t, _, _) -> (AstType.Ident info, t)
    | _ ->
      failwith
        "erreur interne : type_affectable" (* Cas normalement impossible *)
  end
  | AstTds.Deref a -> begin
    let na, ta = analyse_type_affectable a in
    match ta with
    | Pointeur t -> (AstType.Deref na, t)
    | _ -> raise DereferencementIllegal
  end

(* analyse_type_expression : AstTds.expression -> AstType.expression
 * typ *)
(* Paramètre e : l'expression à analyser *)
(* Place les types dans les info_ast de l'AST et associe les bonnes
   opérations en fonction des types dans les expressions *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_expression e =
  let aux_decouple l =
    List.fold_left
      (fun acc x ->
        let x1, x2 = x in
        let acc1, acc2 = acc in
        (acc1 @ [ x1 ], acc2 @ [ x2 ]))
      ([], []) l
  in
  match e with
  | AstTds.AppelFonction (info, le, liste_param_def) -> begin
    match !info with
    | InfoFun (_, tr, argst) ->
      (* On analyse la liste des arguments donnés en paramètre à la fonction couplés aux param par défaut *)
      let list_cpl_ne = List.map analyse_type_expression le in

      (* On découple notre liste de couple d'expression et de types *)
      let nle, tp = aux_decouple list_cpl_ne in

      (* On récupère les param par défaut (On les sort de l'Option) *)
      let nliste_param_def =
        List.fold_right
          (fun x acc ->
            match x with None -> acc | Some paramd -> paramd :: acc)
          liste_param_def []
      in

      (* On vérifie qu'il y a assez de param par défaut pour palier au manque d'arguments *)
      let tailleParam = List.length argst in
      let tailleParamDef = List.length nliste_param_def in
      let tailleArgs = List.length tp in
      if tailleArgs + tailleParamDef < tailleParam && tailleParamDef <> 0 then
        raise MauvaisNombreArguments
      else
        (* on récupère les paramètres par défaut qui vont être utilisés *)
        let used_liste_param_def =
          suppr_deb_liste nliste_param_def
            (tailleParamDef - (tailleParam - tailleArgs))
        in
        let suite_nle, suite_tp =
          aux_decouple (List.map analyse_type_expression used_liste_param_def)
        in
        let final_nle = nle @ suite_nle in
        let final_tp = tp @ suite_tp in
        (* On vérifie que la liste des types des paramètres donné en arguments correspond à ceux attendus *)
        if est_compatible_list final_tp argst then
          (AstType.AppelFonction (info, final_nle), tr)
        else raise (TypesParametresInattendus (tp, argst))
    | _ -> failwith "erreur interne : type_expression - appel fonction"
    (* Cas normalement impossible *)
  end
  | AstTds.Affectable a ->
    (* On analyse l'affectable *)
    let na, ta = analyse_type_affectable a in
    (AstType.Affectable na, ta)
  | AstTds.Unaire (op, e1) -> begin
    (* On analyse l'expression *)
    let ne1, te1 = analyse_type_expression e1 in
    (* On résout la surcharge d'opération unaire*)
    match (te1, op) with
    | Rat, AstSyntax.Numerateur ->
      (AstType.Unaire (AstType.Numerateur, ne1), Int)
    | Rat, AstSyntax.Denominateur ->
      (AstType.Unaire (AstType.Denominateur, ne1), Int)
    | t, _ -> raise (TypeInattendu (t, Rat))
  end
  | AstTds.Binaire (op, e1, e2) -> begin
    let ne1, te1 = analyse_type_expression e1 in
    let ne2, te2 = analyse_type_expression e2 in
    (* On résout la surcharge d'opération binaire *)
    match (op, te1, te2) with
    | Plus, Int, Int -> (AstType.Binaire (PlusInt, ne1, ne2), Int)
    | Plus, Rat, Rat -> (AstType.Binaire (PlusRat, ne1, ne2), Rat)
    | Mult, Int, Int -> (AstType.Binaire (MultInt, ne1, ne2), Int)
    | Inf, Int, Int -> (AstType.Binaire (Inf, ne1, ne2), Bool)
    | Fraction, Int, Int -> (AstType.Binaire (Fraction, ne1, ne2), Rat)
    | Mult, Rat, Rat -> (AstType.Binaire (MultRat, ne1, ne2), Rat)
    | Equ, Int, Int -> (AstType.Binaire (EquInt, ne1, ne2), Bool)
    | Equ, Bool, Bool -> (AstType.Binaire (EquBool, ne1, ne2), Bool)
    | _ -> raise (TypeBinaireInattendu (op, te1, te2))
  end
  | AstTds.Booleen b -> (AstType.Booleen b, Bool)
  | AstTds.Entier i -> (AstType.Entier i, Int)
  | Adresse info -> begin
    match !info with
    | InfoVar (_, t, _, _) -> (AstType.Adresse info, Pointeur t)
    | _ -> failwith "erreur interne : type_expression - adresse"
  end
  | New t -> (AstType.New t, Pointeur t)
  | Null -> (AstType.Null, Pointeur Undefined)

(* analyse_type_instruction : AstTds.instruction -> AstType *)
(* Paramètre i : l'instruction à analyser *)
(* Place les types dans les info_ast de l'AST et associe les bonnes
   opérations en fonction des types dans les instructions *)
(* Erreur si mauvaise utilisation des types *)
let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, info, e) -> begin
    (* traitement de l'expression *)
    let ne, nt = analyse_type_expression e in
    if not (est_compatible t nt) then
      (* les types sont imcompatibles *)
      raise (TypeInattendu (nt, t))
    else
      (* ajout de l'info du type dans l'info_ast *)
      match !info with
      | InfoVar _ ->
        modifier_type_variable nt info;
        AstType.Declaration (info, ne)
      | _ -> failwith "erreur interne : type_instruction - declaration"
    (* Cas normalement impossible *)
  end
  | AstTds.Affectation (a, e) -> begin
    (* analyse de l'expression *)
    let ne, net = analyse_type_expression e in
    (* analyse de l'affectable *)
    let na, nat = analyse_type_affectable a in
    if est_compatible net nat then
      (* Les types sont compatibles *)
      AstType.Affectation (na, ne)
    else (* Les types sont incompatibles *)
      raise (TypeInattendu (net, nat))
  end
  | AstTds.Affichage e -> begin
    (* On analyse l'expression *)
    let ne, nt = analyse_type_expression e in
    (* On resout la surcharge d'affichage *)
    match nt with
    | Int -> AstType.AffichageInt ne
    | Bool -> AstType.AffichageBool ne
    | Rat -> AstType.AffichageRat ne
    | _ ->
      failwith
        ("erreur interne : type_instruction - Affichage " ^ string_of_type nt)
    (* Cas normalement impossible *)
  end
  | AstTds.Conditionnelle (c, t, e) ->
    (* On analyse l'expression *)
    let nc, nct = analyse_type_expression c in
    (* la conditionnelle est forcement de type booléenne *)
    if nct = Bool then
      (* si on a bien un booléen *)
      let nbt = analyse_type_bloc t in
      let nbe = analyse_type_bloc e in
      AstType.Conditionnelle (nc, nbt, nbe)
    else raise (TypeInattendu (nct, Bool))
  | AstTds.TantQue (c, b) ->
    (* On analyse l'expression *)
    let nc, nct = analyse_type_expression c in
    if nct = Bool then
      (* La condition est forcément de type bool *)
      AstType.TantQue (nc, analyse_type_bloc b)
    else raise (TypeInattendu (nct, Bool))
  | AstTds.Retour (e, ia) -> begin
    (* On analyse l'expression et on recupere le type de l'expression que va retourner la fonction
       pour le comparer au type de retour de la fonction *)
    let ne, nt = analyse_type_expression e in
    match !ia with
    | InfoFun (_, typret, _) ->
      if not (est_compatible typret nt) then
        (* Le type de l'expression de retour est incompatible avec le type de retour de la fonction *)
        raise (TypeInattendu (nt, typret))
      else AstType.Retour (ne, ia)
    | _ -> failwith "erreur interne : type_instruction - retour"
    (* Cas normalement impossible *)
  end
  | AstTds.Empty -> AstType.Empty
  | AstTds.StatiqueL (t, info, e) -> begin
    (* traitement de l'expression *)
    let ne, nt = analyse_type_expression e in
    if not (est_compatible t nt) then
      (* les types sont imcompatibles *)
      raise (TypeInattendu (nt, t))
    else
      (* ajout de l'info du type dans l'info_ast *)
      match !info with
      | InfoVar _ ->
        modifier_type_variable nt info;
        AstType.StatiqueL (info, ne)
      | _ -> failwith "erreur interne : type_instruction - declaration"
    (* Cas normalement impossible *)
  end

(* analyse_type_bloc : AstTds.bloc -> AstType.bloc *)
(* Paramètre li : liste d'instructions à analyser *)
(* Place les types dans les info_ast de l'AST et associe les bonnes
   opérations en fonction des types dans les blocs *)
(* Erreur si mauvaise utilisation des types *)
and analyse_type_bloc li = List.map analyse_type_instruction li

(* analyse_type_fonction : AstTds.fonction -> AstType.fonction *)
(* Paramètre : la fonction à analyser *)
(* Place les types dans les info_ast de l'AST et associe les bonnes
   opérations en fonction des types dans les focntions *)
(* Erreur si mauvaise utilisation des types *)
let analyse_type_fonction (AstTds.Fonction (tr, info, lp, li)) =
  let mli = List.map analyse_type_instruction li in
  let listI =
    List.fold_left
      (fun (* On ajoute le type t à l'info i *)
             acc x ->
        let t, i, dexp = x in
        let ndexp =
          match dexp with
          | None -> None
          | Some e -> begin
            let ne, te = analyse_type_expression e in
            if est_compatible t te then Some ne
            else raise (TypeParamDefautInattendu (te, t))
          end
        in
        match !i with
        | InfoVar _ ->
          (* On modifie le type de la ref i *)
          modifier_type_variable t i;
          acc @ [ (i, ndexp) ]
        | _ ->
          failwith
            "erreur interne : type_fonction" (* Cas normalement impossible *))
      [] lp
  in
  match !info with
  | InfoFun (_, _, listT) -> begin
    modifier_type_fonction tr listT info;
    AstType.Fonction (info, listI, mli)
  end
  | _ -> failwith "erreur interne : type_fonction - 2"

(* analyser : AstTds.programme -> AstType.programme *)
(* Paramètre : le programme à analyser *)
(* Place les types dans les info_ast de l'AST et associe les bonnes
   opérations en fonction des types dans le programme *)
(* Erreur si mauvaise utilisation des types *)
let analyser (AstTds.Programme (variablesG, fonctions, prog)) =
  let lvg = analyse_type_bloc variablesG in
  let nfs = List.map analyse_type_fonction fonctions in
  let np = analyse_type_bloc prog in
  AstType.Programme (lvg, nfs, np)
