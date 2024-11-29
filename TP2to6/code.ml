(* Génération d'étiquette à l'aide d'un compteur *)
let getEtiquette =
  let num = ref 0 in
  fun () ->
    num := !num + 1;
    "label" ^ string_of_int !num

let pgcd =
  "pgcd\n\
   LOADL 0\n\
   LOAD (1) -2[LB]\n\
   LOAD (1) -1[LB]\n\
   boucle\n\
   LOAD (1) 5[LB]\n\
   JUMPIF (0) fin\n\
   LOAD (1) 4[LB]\n\
   LOAD (1) 5 [LB]\n\
   SUBR IMod\n\
   STORE (1) 3[LB]\n\
   LOAD (1) 5[LB]\n\
   STORE (1) 4[LB]\n\
   LOAD (1) 3[LB]\n\
   STORE(1) 5[LB]\n\
   JUMP boucle\n\
   fin\n\
   LOAD (1) 4[LB]\n\
   RETURN (1) 2\n\n"

let norm =
  "norm\n\
   LOAD (1) -2[LB]\n\
   LOAD (1) -1[LB]\n\
   CALL (LB) pgcd\n\
   LOAD (1) -2[LB]\n\
   LOAD (1) 3[LB]\n\
   SUBR IDiv\n\
   LOAD (1) -1[LB]\n\
   LOAD (1) 3[LB]\n\
   SUBR IDiv\n\
   RETURN (2) 2\n\n"

let rout =
  "ROut\n\
   LOADL '['\n\
   SUBR COut\n\
   LOAD (1) -2[LB]\n\
   SUBR IOut\n\
   LOADL '/'\n\
   SUBR COut\n\
   LOAD (1) -1[LB]\n\
   SUBR IOut\n\
   LOADL ']'\n\
   SUBR COut\n\
   RETURN (0) 2\n\n"

let radd =
  "RAdd\n\
   LOAD (1) -4[LB]\n\
   LOAD (1) -1[LB]\n\
   SUBR IMul\n\
   LOAD (1) -2[LB]\n\
   LOAD (1) -3[LB]\n\
   SUBR IMul\n\
   SUBR IAdd\n\
   LOAD (1) -3[LB]\n\
   LOAD (1) -1[LB]\n\
   SUBR IMul\n\
   CALL (ST) norm\n\
   RETURN (2) 4\n\n"

let rmul =
  "RMul\n\
   LOAD (1) -4[LB]\n\
   LOAD (1) -2[LB]\n\
   SUBR IMul\n\
   LOAD (1) -3[LB]\n\
   LOAD (1) -1[LB]\n\
   SUBR IMul\n\
   CALL (ST) norm\n\
   RETURN (2) 4\n\n"

(* Entête des fichiers Rat  contenant :
   - un saut vers le programme principal
   - la fonction pgcd nécessaire à la normalisation des rationnels
   - une fonction de normalisation des rationnels
   - les fonctions d'affichage (ROut), d'addition (RAdd) et de multiplication (RMult) de rationnel
*)
let getEntete () = "JUMP main\n\n" ^ pgcd ^ norm ^ rout ^ radd ^ rmul

(*Ecriture dans un fichier *)
let ecrireFichier nom texte =
  let fich = open_out nom in
  output_string fich texte;
  close_out fich
