open Type
open Ast.AstSyntax

(* Exceptions pour la gestion des identificateurs *)
exception DoubleDeclaration of string
exception IdentifiantNonDeclare of string
exception MauvaiseUtilisationIdentifiant of string

(* Exceptions pour le typage *)
(* Le premier type est le type réel, le second est le type attendu *)
exception TypeInattendu of typ * typ
exception TypeParamDefautInattendu of typ * typ
exception TypesParametresInattendus of typ list * typ list
exception TypeBinaireInattendu of binaire * typ * typ
(* les types sont les types réels non compatible avec les signatures connues de l'opérateur *)

(* Exception pour les pointeurs *)
(* DereferencementIllegal : on peut pas déréférencer null *)
exception DereferencementIllegal

(* Utilisation illégale de return dans le programme principal *)
exception RetourDansMain

(* Déclaration illégale d'une variable statique locale dans le main *)
exception StatiqueLDansMain

(* Déclaration d'une fonction avec des paramètres par defaut entre des paramètres qui ne le sont pas *)
exception ParamDefautPasALaFin

(* Il manque des arguments dans l'appel à la fonction*)
exception MauvaisNombreArguments
