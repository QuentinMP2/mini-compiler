open Type

(* Interface des arbres abstraits *)
module type Ast = sig
  type affectable
  type expression
  type instruction
  type fonction
  type programme
end

(* *************************************** *)
(* AST après la phase d'analyse syntaxique *)
(* *************************************** *)
module AstSyntax = struct
  (* Opérateurs unaires de Rat *)
  type unaire = Numerateur | Denominateur

  (* Opérateurs binaires de Rat *)
  type binaire = Fraction | Plus | Mult | Equ | Inf

  (* Affectables de Rat *)
  type affectable =
    (* Accès à un identifiant représenté par son nom *)
    | Ident of string
    (* Déférencement d'un affectable *)
    | Deref of affectable

  (* Expressions de Rat *)
  type expression =
    (* Appel de fonction représenté par le nom de la fonction et la liste des paramètres réels *)
    | AppelFonction of string * expression list
    (* Affectable *)
    | Affectable of affectable
    (* Booléen *)
    | Booleen of bool
    (* Entier *)
    | Entier of int
    (* Opération unaire représentée par l'opérateur et l'opérande *)
    | Unaire of unaire * expression
    (* Opération binaire représentée par l'opérateur, l'opérande gauche et l'opérande droite *)
    | Binaire of binaire * expression * expression
    (* Adresse d'un identifiant (avec le symbole &) *)
    | Adresse of string
    (* Allocation mémoire *)
    | New of typ
    (* NULL *)
    | Null

  (* Instructions de Rat *)
  type bloc = instruction list

  and instruction =
    (* Déclaration de variable représentée par son type, son nom et l'expression d'initialisation *)
    | Declaration of typ * string * expression
    (* Affectation d'une variable représentée par son nom et la nouvelle valeur affectée *)
    | Affectation of affectable * expression
    (* Déclaration d'une constante représentée par son nom et sa valeur (entier) *)
    | Constante of string * int
    (* Affichage d'une expression *)
    | Affichage of expression
    (* Conditionnelle représentée par la condition, le bloc then et le bloc else *)
    | Conditionnelle of expression * bloc * bloc
    (* Boucle TantQue représentée par la conditin d'arrêt de la boucle et le bloc d'instructions *)
    | TantQue of expression * bloc
    (* Return d'une fonction *)
    | Retour of expression
    (* Déclaration d'une variable statique locale *)
    | StatiqueL of typ * string * expression

  (* Structure des fonctions de Rat *)
  (* type de retour - nom - liste des paramètres (association type, nom et potentielle expression par défaut) - corps de la fonction *)
  type fonction =
    | Fonction of typ * string * (typ * string * expression option) list * bloc

  (* Variables Globales *)
  (* Déclaration de variable globale représentée par son type, son nom et l'expression d'initialisation *)
  type variableG = DeclarationG of typ * string * expression

  (* Structure d'un programme Rat *)
  (* liste des variables globales - liste de fonction - programme principal *)
  type programme = Programme of variableG list * fonction list * bloc
end

(* ********************************************* *)
(* AST après la phase d'analyse des identifiants *)
(* ********************************************* *)
module AstTds = struct
  type affectable =
    | Ident of Tds.info_ast
    (* le nom de l'identifiant est remplacé par ses informations *)
    | Deref of affectable

  (* Expressions existantes dans notre langage *)
  (* ~ expression de l'AST syntaxique où les noms des identifiants ont été
     remplacés par les informations associées aux identificateurs *)
  type expression =
    | AppelFonction of Tds.info_ast * expression list * expression option list
    | Affectable of affectable
    | Booleen of bool
    | Entier of int
    | Unaire of AstSyntax.unaire * expression
    | Binaire of AstSyntax.binaire * expression * expression
    | Adresse of Tds.info_ast
    | New of typ
    | Null

  (* instructions existantes dans notre langage *)
  (* ~ instruction de l'AST syntaxique où les noms des identifiants ont été
     remplacés par les informations associées aux identificateurs
     + suppression de nœuds (const) *)
  type bloc = instruction list

  and instruction =
    | Declaration of typ * Tds.info_ast * expression
      (* le nom de l'identifiant est remplacé par ses informations *)
    | Affectation of affectable * expression
    | Affichage of expression
    | Conditionnelle of expression * bloc * bloc
    | TantQue of expression * bloc
    | Retour of expression * Tds.info_ast
      (* les informations sur la fonction à laquelle est associé le retour *)
    | Empty (* les nœuds ayant disparus: Const *)
    | StatiqueL of typ * Tds.info_ast * expression

  (* Structure des fonctions dans notre langage *)
  (* type de retour - informations associées à l'identificateur (dont son nom) - liste des paramètres (association type et information sur les paramètres) - corps de la fonction *)
  type fonction =
    | Fonction of
        typ
        * Tds.info_ast
        * (typ * Tds.info_ast * expression option) list
        * bloc

  (* Structure d'un programme dans notre langage *)
  (* On transforme le type des variables globales en variables classique car elles ont le même traitement *)
  type programme = Programme of bloc * fonction list * bloc
end

(* ******************************* *)
(* AST après la phase de typage    *)
(* ******************************* *)
module AstType = struct
  (* Opérateurs unaires de Rat - résolution de la surcharge *)
  type unaire = Numerateur | Denominateur

  (* Opérateurs binaires existants dans Rat - résolution de la surcharge *)
  type binaire =
    | Fraction
    | PlusInt
    | PlusRat
    | MultInt
    | MultRat
    | EquInt
    | EquBool
    | Inf

  (* Affectables existants dans Rat *)
  (* = affectable de AstTds *)
  type affectable = 
    | Ident of Tds.info_ast 
    | Deref of affectable

  (* Expressions existantes dans Rat *)
  (* = expression de AstTds *)
  type expression =
    | AppelFonction of Tds.info_ast * expression list
    | Affectable of affectable
    | Booleen of bool
    | Entier of int
    | Unaire of unaire * expression
    | Binaire of binaire * expression * expression
    | Adresse of Tds.info_ast
    | New of typ
    | Null

  (* instructions existantes Rat *)
  (* = instruction de AstTds + informations associées aux identificateurs, mises à jour *)
  (* + résolution de la surcharge de l'affichage *)
  type bloc = instruction list

  and instruction =
    | Declaration of Tds.info_ast * expression
    | Affectation of affectable * expression
    | AffichageInt of expression
    | AffichageRat of expression
    | AffichageBool of expression
    | Conditionnelle of expression * bloc * bloc
    | TantQue of expression * bloc
    | Retour of expression * Tds.info_ast
    | Empty (* les nœuds ayant disparus: Const *)
    | StatiqueL of Tds.info_ast * expression

  (* informations associées à l'identificateur (dont son nom), liste des paramètres, corps *)
  type fonction =
    | Fonction of Tds.info_ast * (Tds.info_ast * expression option) list * bloc

  (* Structure d'un programme dans notre langage *)
  type programme = Programme of bloc * fonction list * bloc
end

(* ******************************* *)
(* AST après la phase de placement *)
(* ******************************* *)
module AstPlacement = struct
  (* Affectables existants dans notre langage *)
  (* = affectables de AstType *)
  type affectable = AstType.affectable

  (* Expressions existantes dans notre langage *)
  (* = expression de AstType  *)
  type expression = AstType.expression

  (* instructions existantes dans notre langage *)
  type bloc = instruction list * int (* taille du bloc *)

  and instruction =
    | Declaration of Tds.info_ast * expression
    | Affectation of affectable * expression
    | AffichageInt of expression
    | AffichageRat of expression
    | AffichageBool of expression
    | Conditionnelle of expression * bloc * bloc
    | TantQue of expression * bloc
    | Retour of
        expression * int * int (* taille de retour et taille des paramètres *)
    | Empty (* les nœuds ayant disparus: Const *)
    | StatiqueL of Tds.info_ast * expression

  (* informations associées à l'identificateur (dont son nom), liste de paramètres, corps, expression de retour *)
  (* Plus besoin de la liste des paramètres mais on la garde pour les tests du placements mémoire *)
  type fonction = Fonction of Tds.info_ast * Tds.info_ast list * bloc

  (* Structure d'un programme dans notre langage *)
  type programme = Programme of bloc * fonction list * bloc
end
