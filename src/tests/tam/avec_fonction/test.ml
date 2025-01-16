open Rat
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../../../../tests/runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let tamfile, chan = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  Sys.remove tamfile;
  (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile = print_string (runtamcode runtamcmde ratfile)

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/tam/avec_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)
let%expect_test "testfun1" =
  runtam (pathFichiersRat ^ "testfun1.rat");
  [%expect {| 1 |}]

let%expect_test "testfun2" =
  runtam (pathFichiersRat ^ "testfun2.rat");
  [%expect {| 7 |}]

let%expect_test "testfun3" =
  runtam (pathFichiersRat ^ "testfun3.rat");
  [%expect {| 10 |}]

let%expect_test "testfun4" =
  runtam (pathFichiersRat ^ "testfun4.rat");
  [%expect {| 10 |}]

let%expect_test "testfun5" =
  runtam (pathFichiersRat ^ "testfun5.rat");
  [%expect {| |}]

let%expect_test "testfun6" =
  runtam (pathFichiersRat ^ "testfun6.rat");
  [%expect {|truetrue|}]

let%expect_test "testfuns" =
  runtam (pathFichiersRat ^ "testfuns.rat");
  [%expect {| 28 |}]

let%expect_test "factrec" =
  runtam (pathFichiersRat ^ "factrec.rat");
  [%expect {| 120 |}]

let%expect_test "testfunP1" =
  runtam (pathFichiersRat ^ "testfunP1.rat");
  [%expect {| 75 |}]

let%expect_test "testfunP2" =
  runtam (pathFichiersRat ^ "testfunP2.rat");
  [%expect {| 69 |}]

let%expect_test "testVG1" =
  runtam (pathFichiersRat ^ "testVG1.rat");
  [%expect {| false10 |}]

let%expect_test "testVG2" =
  runtam (pathFichiersRat ^ "testVG2.rat");
  [%expect {| 48 |}]

let%expect_test "testVG3" =
  runtam (pathFichiersRat ^ "testVG3.rat");
  [%expect {| 336 |}]

let%expect_test "testVG4" =
  runtam (pathFichiersRat ^ "testVG4.rat");
  [%expect {| 44 |}]

let%expect_test "testVG5" =
  runtam (pathFichiersRat ^ "testVG5.rat");
  [%expect {| 13574 |}]

let%expect_test "testSL1" =
  runtam (pathFichiersRat ^ "testSL1.rat");
  [%expect {| 136 |}]

let%expect_test "testSL2" =
  runtam (pathFichiersRat ^ "testSL2.rat");
  [%expect {| 136 |}]

let%expect_test "testSL3" =
  runtam (pathFichiersRat ^ "testSL3.rat");
  [%expect {| truetruetrue |}]

let%expect_test "testSL4" =
  runtam (pathFichiersRat ^ "testSL4.rat");
  [%expect {| 1234 |}]

let%expect_test "testPD1" =
  runtam (pathFichiersRat ^ "testPD1.rat");
  [%expect {| 10203040102030410203410234 |}]

let%expect_test "testALL" =
  runtam (pathFichiersRat ^ "testALL.rat");
  [%expect {| 457810 |}]

let%expect_test "testALL2" =
  runtam (pathFichiersRat ^ "testALL2.rat");
  [%expect {| 60 |}]
