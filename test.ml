(* -------------------------------------------------------------------------- *)
(* ----------------------- TP1 - IFT-3000 - Hiver 2018 ---------------------- *)
(* -------------------------------------------------------------------------- *)

#use "ptree.ml";;

module TestTp1 = struct

  open PTree
  open List

(* Structures de données et fonctions utiles pour tests --------------------- *)
  type rules = 
    ID | IMP | ET | OU | H | TR | SYM   (* vous pouvez ajouter des constantes *)

(* Exemple de fonction permettant de donner une version string d'une règle    *)
  let r2str r = 
    match r with
    | ID -> "(id)"
    | IMP -> "(imp)"
    | ET -> "(et)"
    | OU -> "(ou)"
    | H -> "(u)"
    | TR -> "(tr)"
    | SYM -> "(sym)"

(* Fonctions de tests ------------------------------------------------------- *)
  let gen pt =
    let t = ptree2stree (fun x -> x) r2str pt in 
    genstree t

  let gen' ?(l=0) pt =
    let t = ptree2stree (fun x -> x) r2str pt in 
    genltree ~l:l t

end

(*
#use "test.ml";;

open PTree;;
open TestTp1;;

let a = PT(IMP, "A v A", [PT(OU,"A", [PT(H,"A v A",[])])]);;
gen a;;

let a' = PT(IMP, "P \\vee P \\Rightarrow P", [PT(OU,"P", [PT(ID,"P",[]); PT(ID,"P",[])])]);;
gen' a';;

let a = PT(IMP, "x1 = x2 ET x1 = x3 => x2 = x3", [PT(TR,"x2 = x3", [PT(SYM,"x2 = x1",[PT(ET,"x1 = x2",[PT(H,"x1 = x2 ET x_1 = x_3",[])])]); PT(ET,"x1 = x3",[PT(H,"x1 = x2 ET x1 = x3",[])])])]);;
gen a;;

let a' = PT(IMP, "x_1 = x_2 \\wedge x_1 = x_3 \\Rightarrow x_2 = x_3", [PT(TR,"x_2 = x_3", [PT(SYM,"x_2 = x_1",[PT(ET,"x_1 = x_2",[PT(H,"x_1 = x_2 \\wedge x_1 = x_3",[])])]); PT(ET,"x_1 = x_3",[PT(H,"x_1 = x_2 \\wedge x_1 = x_3",[])])])]);;
gen' a';;
gen' ~l:1 a';;
gen' ~l:2 a';;
gen' ~l:3 a';;
gen' ~l:4 a';;
*)
