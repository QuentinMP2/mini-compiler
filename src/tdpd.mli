open Ast

(* Module de table des paramètres par défaut *)
(* Permet d'enregistrer les paramètres par défaut d'une fonction pour ensuite les faire passer aux appels à la-dite fonction *)

(* Table des paramètres par défaut *)
type tdpd

(* Crée une table des paramètres par défaut vide *)
val creerTDPD : unit -> tdpd

(* Ajoute une liste de valeurs par défaut dans la table des paramètres par défaut *)
(* Le premier paramètre est le nom de la fonction *)
(* Le deuxième paramètre est sa liste de paramètre par défaut *)
(* Le troisième paramètre est la table des paramètres par défaut *)
val ajouterTDPD : string -> AstTds.expression option list -> tdpd -> unit

(* Récupère la liste des paremètres par défaut d'une fonction *)
(* Le premier paramètre est le nom de la fonction *)
(* Le deuxième paramètre est la table des paramètres par défaut *)
val recupTDPD : string -> tdpd -> AstTds.expression option list

(* transforme la tdpd en string *)
(* Le premier paramètre est la table des paramètres par défaut *)
val toString : tdpd -> string
