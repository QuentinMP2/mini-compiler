(* Types manipulés dans Rat *)
type typ = Bool | Int | Rat | Undefined | Pointeur of typ

(* string_of_type :  typ -> string *)
(* transforme un typ en chaîne de caractère *)
val string_of_type : typ -> string

(* est_compatible : typ -> typ -> bool *)
(* vérifie que le second type est compatible avec le premier *)
(* c'est à dire qu'un élèment du second type peut être affecté *)
(* à un élément du premier type *)
val est_compatible : typ -> typ -> bool

(* est_compatible_list : typ list -> typ list -> bool *)
(* vérifie si les types sont compatibles deux à deux *)
val est_compatible_list : typ list -> typ list -> bool

(* getTaille : typ -> int *)
(* Renvoie la taille en mémoire qui doit prendre une variable en fonction de son type *)
val getTaille : typ -> int

(* type_prof : typ -> int -> typ *)
(* Renvoie le type à la profondeur indiquée d'une variable *)
val type_prof : typ -> int -> typ

(* suppr_deb_liste : 'a list -> int -> list *)
(* supprime les n premiers elements d'une liste *)
val suppr_deb_liste : 'a list -> int -> 'a list
