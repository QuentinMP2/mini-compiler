open Tds
open Exceptions
open Ast
open Type

type t1 = Ast.AstTds.programme
type t2 = Ast.AstType.programme

let rec analyse_type_expression e =
  match e with
  | AstTds.AppelFonction (info, le) -> (
      match !info with
      | InfoFun (_, tr, argst) ->
          let list_cpl_ne = List.map analyse_type_expression le in
          let nle, tp =
            List.fold_left
              (fun acc x ->
                let x1, x2 = x in
                let acc1, acc2 = acc in
                (acc1 @ [ x1 ], acc2 @ [ x2 ]))
              ([], []) list_cpl_ne
          in
          if est_compatible_list tp argst then
            (AstType.AppelFonction (info, nle), tr)
          else raise (TypesParametresInattendus (tp, argst))
      | _ -> failwith "erreur interne")
  | AstTds.Ident info -> (
      match !info with
      | InfoVar (_, typ, _, _) -> (AstType.Ident info, typ)
      | _ -> failwith "erreur interne")
  | AstTds.Unaire (op, e1) -> (
      let ne1, te1 = analyse_type_expression e1 in
      match (te1, op) with
      | Rat, AstSyntax.Numerateur ->
          (AstType.Unaire (AstType.Numerateur, ne1), Int)
      | Rat, AstSyntax.Denominateur ->
          (AstType.Unaire (AstType.Denominateur, ne1), Int)
      | t, _ -> raise (TypeInattendu (t, Rat)))
  | AstTds.Binaire (op, e1, e2) -> (
      let ne1, te1 = analyse_type_expression e1 in
      let ne2, te2 = analyse_type_expression e2 in
      match (op, te1, te2) with
      | Plus, Int, Int -> (AstType.Binaire (PlusInt, ne1, ne2), Int)
      | Plus, Rat, Rat -> (AstType.Binaire (PlusRat, ne1, ne2), Rat)
      | Mult, Int, Int -> (AstType.Binaire (MultInt, ne1, ne2), Int)
      | Inf, Int, Int -> (AstType.Binaire (Inf, ne1, ne2), Bool)
      | Fraction, Int, Int -> (AstType.Binaire (Fraction, ne1, ne2), Rat)
      | Mult, Rat, Rat -> (AstType.Binaire (MultRat, ne1, ne2), Rat)
      | Equ, Int, Int -> (AstType.Binaire (EquInt, ne1, ne2), Bool)
      | Equ, Bool, Bool -> (AstType.Binaire (EquBool, ne1, ne2), Bool)
      | _ -> raise (TypeBinaireInattendu (op, te1, te2)))
  | AstTds.Booleen b -> (AstType.Booleen b, Bool)
  | AstTds.Entier i -> (AstType.Entier i, Int)

let rec analyse_type_instruction i =
  match i with
  | AstTds.Declaration (t, info, e) -> (
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
        | _ -> failwith "erreur interne")
  | AstTds.Affectation (info, e) -> (
      let ne, nt = analyse_type_expression e in
      match !info with
      | InfoVar (_, typ, _, _) ->
          if not (est_compatible nt typ) then raise (TypeInattendu (nt, typ))
          else AstType.Affectation (info, ne)
      | _ -> failwith "erreur interne")
  | AstTds.Affichage e -> (
      let ne, netyp = analyse_type_expression e in
      match netyp with
      | Int -> AstType.AffichageInt ne
      | Bool -> AstType.AffichageBool ne
      | Rat -> AstType.AffichageRat ne
      | _ -> failwith "erreur interne" (*car ne devrait pas être possible *))
  | AstTds.Conditionnelle (c, t, e) ->
      let nc, nctyp = analyse_type_expression c in
      if nctyp = Bool then
        let nbt = analyse_type_bloc t in
        let nbe = analyse_type_bloc e in
        AstType.Conditionnelle (nc, nbt, nbe)
      else raise (TypeInattendu (nctyp, Bool))
  | AstTds.TantQue (c, b) ->
      let nc, nctyp = analyse_type_expression c in
      if nctyp = Bool then AstType.TantQue (nc, analyse_type_bloc b)
      else raise (TypeInattendu (nctyp, Bool))
  | AstTds.Retour (e, ia) -> (
      let ne, netyp = analyse_type_expression e in
      match !ia with
      | InfoFun (_, typret, _) ->
          if not (est_compatible typret netyp) then
            raise (TypeInattendu (netyp, typret))
          else AstType.Retour (ne, ia)
      | _ -> failwith "erreur interne")
  | AstTds.Empty -> AstType.Empty

and analyse_type_bloc li = List.map analyse_type_instruction li

let analyse_type_fonction (AstTds.Fonction (_, info, lp, li)) =
  let mli = List.map analyse_type_instruction li in
  let listI : info ref list =
    List.fold_left
      (fun (* On ajoute le type t à l'info i *)
             acc x ->
        let t, i = x in
        match !i with
        | InfoVar (nom, _, depl, nomReg) ->
            let ni = ref (InfoVar (nom, t, depl, nomReg)) in
            acc @ [ ni ]
        | _ -> failwith "erreur interne")
      [] lp
  in
  AstType.Fonction (info, listI, mli)

let analyse_type_fonctions lf = List.map analyse_type_fonction lf

let analyser (AstTds.Programme (fonctions, prog)) =
  let nfs = analyse_type_fonctions fonctions in
  let np = analyse_type_bloc prog in
  AstType.Programme (nfs, np)
