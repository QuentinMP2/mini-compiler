open Hashtbl
open Ast
open PrinterAst

(* Table des paramètres par défaut *)
type tdpd = (string, AstTds.expression option list) Hashtbl.t

(* creerTDPD : () -> tdpd *)
(* Crée une Table des paramètres par défaut vide *)
let creerTDPD () = Hashtbl.create 256

(* ajouterTDPD : string -> (AstTds.expression option) list -> tdpd -> () *)
(* Paramètre nomf : le nom de la fonction à ajouter dans la table *)
(* Paramètre ldp : liste des param par defaut associé à la fonction *)
(* Paramètre tdpd : la table des paramètres par défaut *)
(* Ajoute à la table des paramètres par défaut la liste des param par défaut associé à nomf *)
let ajouterTDPD nomf lpd tdpd = Hashtbl.add tdpd nomf lpd

(* recupTDPD : string -> (AstTds.expression option) list *)
(* Paramètre nomf : le nom de la fonction pour laquelle on veut récup sa liste de param par défaut *)
(* Paramètre tdpd : la table des paramètres par défaut *)
(* récupère dans la table des paramètres par défaut la liste associé à nomf *)
let recupTDPD nomf tdpd =
  match find_opt tdpd nomf with
  | Some lpd -> lpd
  | None -> failwith "erreur interne : recupTDPD"

(* TESTS *)
let%test _ =
  let tdpd = creerTDPD () in
  let liste_de_param_defaut1 =
    [ None; Some (AstTds.Entier 3); Some (AstTds.Booleen true) ]
  in
  let liste_de_param_defaut2 =
    [
      Some (AstTds.Binaire (Fraction, AstTds.Entier 1, AstTds.Entier 2));
      Some (AstTds.Booleen true);
      Some (AstTds.Booleen true);
    ]
  in
  ajouterTDPD "f" liste_de_param_defaut1 tdpd;
  ajouterTDPD "g" liste_de_param_defaut2 tdpd;
  recupTDPD "f" tdpd = liste_de_param_defaut1

let%test _ =
  let tdpd = creerTDPD () in
  let liste_de_param_defaut1 =
    [ None; Some (AstTds.Entier 3); Some (AstTds.Booleen true) ]
  in
  let liste_de_param_defaut2 =
    [
      Some (AstTds.Binaire (Fraction, AstTds.Entier 1, AstTds.Entier 2));
      Some (AstTds.Booleen true);
      Some (AstTds.Booleen true);
    ]
  in
  ajouterTDPD "f" liste_de_param_defaut1 tdpd;
  ajouterTDPD "g" liste_de_param_defaut2 tdpd;
  recupTDPD "g" tdpd = liste_de_param_defaut2

(* toString : tdpd -> string *)
(* Paramètre tdpd : la table des paramètres par défaut *)
let toString (tdpd : tdpd) =
  Hashtbl.fold
    (fun n ldp accu ->
      accu ^ n ^ " : "
      ^ List.fold_left
          (fun acc x ->
            let new_str =
              match x with
              | None -> ""
              | Some e -> "@param=" ^ PrinterAstTds.string_of_expression e ^ " "
            in
            acc ^ new_str)
          "" ldp
      ^ "/")
    tdpd ""

(* TESTS *)
let%test _ =
  let tdpd = creerTDPD () in
  ajouterTDPD "f" [ Some (AstTds.Entier 3); Some (AstTds.Booleen true) ] tdpd;
  toString tdpd = "f : @param=3  @param=true  /"
