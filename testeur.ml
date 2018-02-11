(* --------------------------------------------------------------------------- *)
(* ----------------------- TP1 - IFT-3000 - Hiver 2017 ----------------------- *)
(* --------------------------------------------------------------------------- *)
(* Fichier pertmettant de tester les fonctions implantées du TP                *)
(* --------------------------------------------------------------------------- *)
(* On suppose que le Tp (ptree.ml) est chargé dans la mémoire de l'interpréteur*)
(* il suffit alors d'entrer dans l'interpréteur ce qui suit:                   *)
(*                                                                             *)
(* # #use "testeur.ml";;                                                       *)
(*                                                                             *)
(* Par la suite:                                                               *)
(*                                                                             *)
(* # test();;  (* Teste toutes les fonctions                           *)      *)
(* # testn();; (* n = 1 ou 2 ...; teste la fonction numéro n           *)      *)
(*                                                                             *)
(* Lorsque le fichier ptree.ml est modifié, vous n'avez juste qu'à:            *)
(* - recharger le fichier ptree.ml;                                            *)
(* - recharger le fichier testeur.ml.                                          *)
(* Par la suite, vous pouvez de nouveau effectuer les tests (test(); testn())  *)
(* --------------------------------------------------------------------------- *)

open PTree;; 

exception Erreur of string;;                  

(* --------------------------------------------------------------------------- *)
(* -- FONCTIONS UTILES ------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)
let (++) liste liste' =
  List.rev
    (List.fold_left (fun acc e -> if List.mem e liste' then e::acc else acc)
                    [] liste);;               
 
let fail () = false 
and pass () = true;;
  
let assert_equal nom_test a b = 
  if a = b then pass() else fail();;
  
let assert_equal_list nom_test a b = 
  if (a ++ b) = a then pass() else fail();;
  
let assert_throw_exception nom_test lazy_expression =
  match lazy_expression () with
  | exception (Erreur _) -> pass()
  | exception _ -> fail()
  | _ -> fail();;
  
let assert_true nom_test lazy_expression =
  match lazy_expression () with
  | true -> pass()
  | false -> fail();;  
  

(* -- À IMPLANTER/COMPLÉTER (8 PTS) --------------------------------------- *)
(* @Fonction      : includeSep : string -> string list -> string            *)
(* @Description   : retourne une liste d'elts en format string, séparés par
                    une valeur passée en argument                           *)
(* @Precondition  : aucune                                                  *)
(* @Postcondition : aucune                                                  *)
let test1()=
  let note = ref 0. in
  let comment = ref [] in

  try
    if assert_equal "includeSep@CasChaineVideListeVide" (includeSep "" []) ""  
    then note := !note +. 0.5 
    else comment := (!comment) @ [ "CasChaineVideListeVide incorrect!" ];

    if assert_equal "includeSep@CasChaineVide" 
                    (includeSep "" ["a"],includeSep "" ["a"; "b"]) ("a","ab")  
    then note := !note +. 0.5 
    else comment := (!comment) @ [ "CasChaineVide incorrect!" ];
 
    if assert_equal "includeSep@CasChaineNonVideListeVide" 
                    (includeSep "---" []) "---"  
    then note := !note +. 1. 
    else comment := (!comment) @ [ "CasChaineNonVideListeVide incorrect!" ];

    if assert_equal "includeSep@CasChaineNonVideListeUn" 
                    (includeSep "---" ["a"]) "---a---"  
    then note := !note +. 1. 
    else comment := (!comment) @ [ "CasChaineNonVideListeUn incorrect!" ];

    if assert_equal "includeSep@CasChaineNonVideListeDeux" 
                    (includeSep "---" ["a";"b"]) "---a---b---"  
    then note := !note +. 2. 
    else comment := (!comment) @ [ "CasChaineNonVideListeDeux incorrect!" ];

    if assert_equal "includeSep@CasChaineNonVideListeTrois" 
                    (includeSep "---" ["a";"b";"c"]) "---a---b---c---"  
    then note := !note +. 3. 
    else comment := (!comment) @ [ "CasChaineNonVideListeTrois incorrect!" ];

    (!note, !comment, false)

  with
  | Non_Implante _ -> 
    comment := (!comment) @ [ "Non_implantee" ]; (!note, !comment, true)
  | _ -> 
    comment := (!comment) @ [ "Test_non_complete" ]; (!note, !comment, true);;
  

(* -- À IMPLANTER/COMPLÉTER (12 PTS) -------------------------------------- *)
(* @Fonction      : height : strTree -> int                                 *)
(* @Description   : retourne la taille d'un arbre passé en argument         *)
(* @Precondition  : aucune                                                  *)
(* @Postcondition : la valeur retournée est positive et correspond à taille *)
let test2()=
  let note = ref 0. in
  let comment = ref [] in

  try

    let a1 = St 1 in
    let a2 = Leaf "a" in 
    let a3 = Tree("a","b",[]) in
    let a4 = Tree("a","b",[a1]) in 
    let a5 = Tree("a","b",[a1;a2]) in
    let a5' = Tree("a","b",[a1;a1]) in
    let a5'' = Tree("a","b",[a2;a2]) in
    let a6 = Tree("a","b",[a1;a4]) in
    let a7 = Tree("a","b",[a6;a4]) in

    if assert_equal "height@CasCst" 
                    (height a1, height a2) (0,0)  
    then note := !note +. 1.
    else comment := (!comment) @ [ "CasCst incorrect!" ];

    if assert_equal "height@CasFilsVide" (height a3) 1  
    then note := !note +. 1. 
    else comment := (!comment) @ [ "CasFilsVide incorrect!" ];

    if assert_equal "height@CasFilsUn" (height a4) 1  
    then note := !note +. 1. 
    else comment := (!comment) @ [ "CasFilsUn incorrect!" ];

    if assert_equal "height@CasFilsDeux" 
                    (height a5,height a5',height a5'') (1,1,1)  
    then note := !note +. 2. 
    else comment := (!comment) @ [ "CasFilsDeux incorrect!" ];

    if assert_equal "height@CasHaut2" (height a6) 2  
    then note := !note +. 3. 
    else comment := (!comment) @ [ "CasHaut2 incorrect!" ];

    if assert_equal "height@CasHaut3" (height a7) 3  
    then note := !note +. 4. 
    else comment := (!comment) @ [ "CasHaut3 incorrect!" ];

    (!note, !comment, false)

  with
  | Non_Implante _ -> 
    comment := (!comment) @ [ "Non_implantee" ]; (!note, !comment, true)
  | _ -> 
    comment := (!comment) @ [ "Test_non_complete" ]; (!note, !comment, true);;
 

(* -- À IMPLANTER/COMPLÉTER (20 PTS) -------------------------------------- *)
(* @Fonction      : ptree2stree : ('formula -> string)-> ('rule -> string) -> 
                                  ('formula, 'rule) pTree -> strTree        *)
(* @Description   : traduit un arbre prrofTree en un arbre strTree          *)
(* @Precondition  : aucune                                                  *)
(* @Postcondition : l'arbre retourné est correct                            *)
let test3()=
  let note = ref 0. in
  let comment = ref [] in

  try

    let a1 = PF "a" in
    let a2 = PT("a","a",[]) in 
    let a3 = PT(1,"a",[]) in
    let a4 = PT(1,2,[]) in 
    let a5 = PT("a","b",[PF "a"]) in
    let a6 = PT("a","b",[PF "a"; PF "a"]) in
    let a7 = PT("a","b",[a5]) in

    let obt1 = ptree2stree (fun x -> x) (fun x -> x) a1 in
    begin
      if assert_equal "ptree2stree@CasPF" 
         obt1 (Leaf "a")  
      then note := !note +. 1.
      else comment := (!comment) @ [ "CasPF incorrect!" ];
    end;

    let obt1 = ptree2stree (fun x -> x) (fun x -> x) a2 in
    begin
      if assert_equal "ptree2stree@CasPTVide" 
         obt1 (Tree ("a", "a", []))  
      then note := !note +. 1. 
      else comment := (!comment) @ [ "CasPTVide incorrect!" ];
    end;

    let obt1 = ptree2stree (fun x -> x) string_of_int a3 in
    begin
      if assert_equal "ptree2stree@CasPTVide2" 
         obt1 (Tree ("1", "a", []))  
      then note := !note +. 2. 
      else comment := (!comment) @ [ "CasPTVide2 incorrect!" ];
    end;

    let obt1 = ptree2stree string_of_int string_of_int a4 in
    begin
      if assert_equal "ptree2stree@CasPTVide3" 
         obt1 (Tree ("1", "2", []))  
      then note := !note +. 3. 
      else comment := (!comment) @ [ "CasPTVide3 incorrect!" ];
    end;

    let obt1 = ptree2stree (fun x -> x) (fun x -> x) a5 in
    begin
      if assert_equal "ptree2stree@CasPT1" 
         obt1 (Tree ("a", "b", [Leaf "a"]))  
      then note := !note +. 4. 
      else comment := (!comment) @ [ "CasPT1 incorrect!" ];
    end;

    let obt1 = ptree2stree (fun x -> x) (fun x -> x) a6 in
    begin
      if assert_equal "ptree2stree@CasPT2" 
         obt1 (Tree ("a", "b", [Leaf "a"; Leaf "a"]))  
      then note := !note +. 4. 
      else comment := (!comment) @ [ "CasPT2 incorrect!" ];
    end;

    let obt1 = ptree2stree (fun x -> x) (fun x -> x) a7 in
    begin
      if assert_equal "ptree2stree@CasPT12" 
         obt1 (Tree ("a", "b", [Tree ("a", "b", [Leaf "a"])]))  
      then note := !note +. 5. 
      else comment := (!comment) @ [ "CasPT12 incorrect!" ];
    end;

    (!note, !comment, false)

  with
  | Non_Implante _ -> 
    comment := (!comment) @ [ "Non_implantee" ]; (!note, !comment, true)
  | _ -> 
    comment := (!comment) @ [ "Test_non_complete" ]; (!note, !comment, true);;
 

(* -- À IMPLANTER/COMPLÉTER (40 PTS) -------------------------------------- *)
(* @Fonction      : tree2mtree : ?l:int->strTree->(int * strTree) list      *)
(* @Description   : transforme un arbre en liste de sous-arbres             *)
(* @Precondition  : «l» doit être positive ou nulle                         *)
(* @Postcondition : les arbres retournées sont correctement liées           *)
let test4()=
  let note = ref 0. in
  let comment = ref [] in

  try

    let a1 = St 1 in
    let a2 = Leaf "a" in 
    let a3 = Tree("a","b",[]) in
    let a4 = Tree("a","b",[a1]) in 
    let a5 = Tree("a","b",[a1;a4]) in
    let a6 = Tree("a","b",[a5;a4]) in
    let a7 = Tree("a","b",[a6;a4;a5]) in

    let obt1,obt2 = tree2mtree a1, tree2mtree ~l:1 a1 in
    begin
      if assert_equal "tree2mtree@CasSt" 
         (obt1,obt2) ([],[])  
      then note := !note +. 3.
      else comment := (!comment) @ [ "CasSt incorrect!" ];
    end;

    let obt1,obt2 = tree2mtree a2, tree2mtree ~l:1 a2 in
    begin
      if assert_equal "tree2mtree@CasLeaf" 
         (obt1,obt2) ([],[])   
      then note := !note +. 3. 
      else comment := (!comment) @ [ "CasLeaf incorrect!" ];
    end;

    let obt1,obt2 = tree2mtree a3, tree2mtree ~l:1 a3 in
    begin
      if assert_equal "tree2mtree@CasFilsVide" 
         (obt1,obt2) ([(1, Tree ("a", "b", []))],[(1, Tree ("a", "b", []))])  
      then note := !note +. 4. 
      else comment := (!comment) @ [ "CasFilsVide incorrect!" ];
    end;

    let obt1,obt2,obt3 = tree2mtree a4, tree2mtree ~l:1 a4, tree2mtree ~l:2 a4 in
    begin
      if assert_equal "tree2mtree@CasFilsUn" 
         (obt1,obt2,obt3) ([(1, Tree ("a", "b", [St 1]))],[(1, Tree ("a", "b", [St 1]))],[(1, Tree ("a", "b", [St 1]))])  
      then note := !note +. 5. 
      else comment := (!comment) @ [ "CasFilsUn incorrect!" ];
    end;

    let obt1,obt2,obt3 = tree2mtree a5, tree2mtree ~l:1 a5, tree2mtree ~l:2 a5 in
    begin
      if assert_equal "tree2mtree@CasFil2" 
         (obt1,obt2,obt3) ([(1, Tree ("a", "b", [St 1; St 2])); (2, Tree ("a", "b", [St 1]))], [(1, Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]))], [(1, Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]))])  
      then note := !note +. 7. 
      else comment := (!comment) @ [ "CasFils2 incorrect!" ];
    end;

    let obt1,obt2,obt3,obt4 = tree2mtree a6, tree2mtree ~l:1 a6, tree2mtree ~l:2 a6, tree2mtree ~l:3 a6 in
    begin
      if assert_equal "tree2mtree@CasFils2" 
         (obt1,obt2,obt3,obt4) ([(1, Tree ("a", "b", [St 2; St 4])); (2, Tree ("a", "b", [St 1; St 3])); (3, Tree ("a", "b", [St 1])); (4, Tree ("a", "b", [St 1]))],
                                [(1, Tree ("a", "b", [St 2; Tree ("a", "b", [St 1])])); (2, Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]))],
                                [(1, Tree ("a", "b", [Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]); Tree ("a", "b", [St 1])]))],
                                [(1, Tree ("a", "b", [Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]); Tree ("a", "b", [St 1])]))])
      then note := !note +. 8. 
      else comment := (!comment) @ [ "CasFils2 incorrect!" ];
    end;

    let obt1,obt2,obt3,obt4,obt5 = tree2mtree a7, tree2mtree ~l:1 a7, tree2mtree ~l:2 a7, tree2mtree ~l:3 a7, tree2mtree ~l:4 a7 in
    begin
      if assert_equal "tree2mtree@CasFils3" 
         (obt1,obt2,obt3,obt4,obt5) ([(1, Tree ("a", "b", [St 2; St 6; St 7])); (2, Tree ("a", "b", [St 3; St 5])); (3, Tree ("a", "b", [St 1; St 4]));(4, Tree ("a", "b", [St 1])); (5, Tree ("a", "b", [St 1]));(6, Tree ("a", "b", [St 1])); (7, Tree ("a", "b", [St 1; St 8]));(8, Tree ("a", "b", [St 1]))],
                                     [(1, Tree ("a", "b", [St 2; Tree ("a", "b", [St 1]); St 4]));(2, Tree ("a", "b", [St 3; Tree ("a", "b", [St 1])]));(3, Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]));(4, Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]))],
                                     [(1, Tree ("a", "b", [St 2; Tree ("a", "b", [St 1]); Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])])])); (2, Tree ("a", "b", [Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]); Tree ("a", "b", [St 1])]))],
                                     [(1, Tree ("a", "b", [Tree ("a", "b", [Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]); Tree ("a", "b", [St 1])]); Tree ("a", "b", [St 1]); Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])])]))],
                                     [(1, Tree ("a", "b", [Tree ("a", "b", [Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])]); Tree ("a", "b", [St 1])]); Tree ("a", "b", [St 1]); Tree ("a", "b", [St 1; Tree ("a", "b", [St 1])])]))])
      then note := !note +. 10. 
      else comment := (!comment) @ [ "CasFils3 incorrect!" ];
    end;

    (!note, !comment, false)

  with
  | Non_Implante _ -> 
    comment := (!comment) @ [ "Non_implantee" ]; (!note, !comment, true)
  | _ -> 
    comment := (!comment) @ [ "Test_non_complete" ]; (!note, !comment, true);;


(* -- À IMPLANTER/COMPLÉTER (20 PTS) -------------------------------------- *)
(* @Fonction      : mtree2pretty : (strTree -> string) -> (int -> string) -> 
                                   string->(int * strTree) list->string list*)
(* @Description   : retourne une liste de chaines de caractères correspondant
                    à l'arbre, ou aux arbres, à afficher                    *)
(* @Precondition  : aucune                                                  *)
(* @Postcondition : aucune                                                  *)
let test5()=
  let note = ref 0. in
  let comment = ref [] in

  let mtree2pretty' = mtree2pretty tree2str id2str "\n" in

  try

    let lf1 = [] in
    let lf2 = [(1, Tree ("a", "b", []))] in 
    let lf3 = [(1, Tree ("a", "b", [St 1]))] in
    let lf4 = [(1, Tree ("a", "b", [St 1; St 2])); (2, Tree ("a", "b", [St 1]))] in 
    let lf5 = [(1, Tree ("a", "b", [St 2; St 4])); (2, Tree ("a", "b", [St 1; St 3])); (3, Tree ("a", "b", [St 1])); (4, Tree ("a", "b", [St 1]))] in
    let lf6 = [(1, Tree ("a", "b", [St 2; St 6; St 7])); (2, Tree ("a", "b", [St 3; St 5])); (3, Tree ("a", "b", [St 1; St 4]));(4, Tree ("a", "b", [St 1])); (5, Tree ("a", "b", [St 1]));(6, Tree ("a", "b", [St 1])); (7, Tree ("a", "b", [St 1; St 8]));(8, Tree ("a", "b", [St 1]))] in

    let obt1 = mtree2pretty' lf1 in
    begin
      if assert_equal "mtree2pretty@CasVide" 
         obt1 []  
      then note := !note +. 1.
      else comment := (!comment) @ [ "CasVide incorrect!" ];
    end;

    let obt1 = mtree2pretty' lf2 in
    begin
      if assert_equal "mtree2pretty@CasFilsVide" 
         obt1 ["\t   \n\t--- a\n\t b"]  
      then note := !note +. 2. 
      else comment := (!comment) @ [ "CasFilsVide incorrect!" ];
    end;

    let obt1 = mtree2pretty' lf3 in
    begin
      if assert_equal "mtree2pretty@CasFilsUn" 
         obt1 ["\t   A1   \n\t-------- a\n\t   b"]  
      then note := !note +. 2. 
      else comment := (!comment) @ [ "CasFilsUn incorrect!" ];
    end;

    let obt1 = mtree2pretty' lf4 in
    begin
      if assert_equal "mtree2pretty@CasArbre2" 
         obt1 ["A1:"; "\t   A1   A2   \n\t------------- a\n\t      b\n"; 
               "A2:"; "\t   A1   \n\t-------- a\n\t   b\n"] 
      then note := !note +. 4. 
      else comment := (!comment) @ [ "CasArbre2 incorrect!" ];
    end;

    let obt1 = mtree2pretty' lf5 in
    begin
      if assert_equal "mtree2pretty@CasArbre4" 
         obt1 ["A1:"; "\t   A2   A4   \n\t------------- a\n\t      b\n"; 
               "A2:"; "\t   A1   A3   \n\t------------- a\n\t      b\n"; 
               "A3:"; "\t   A1   \n\t-------- a\n\t   b\n"; 
               "A4:"; "\t   A1   \n\t-------- a\n\t   b\n"]
      then note := !note +. 5. 
      else comment := (!comment) @ [ "CasArbre4 incorrect!" ];
    end;

    let obt1 = mtree2pretty' lf6 in
    begin
      if assert_equal "mtree2pretty@CasArbre8" 
         obt1 ["A1:"; "\t   A2   A6   A7   \n\t------------------ a\n\t        b\n"; "A2:";
               "\t   A3   A5   \n\t------------- a\n\t      b\n"; "A3:";
               "\t   A1   A4   \n\t------------- a\n\t      b\n"; "A4:";
               "\t   A1   \n\t-------- a\n\t   b\n"; "A5:";
               "\t   A1   \n\t-------- a\n\t   b\n"; "A6:";
               "\t   A1   \n\t-------- a\n\t   b\n"; "A7:";
               "\t   A1   A8   \n\t------------- a\n\t      b\n"; "A8:";
               "\t   A1   \n\t-------- a\n\t   b\n"]
      then note := !note +. 6. 
      else comment := (!comment) @ [ "CasArbre8 incorrect!" ];
    end;

    (!note, !comment, false)

  with
  | Non_Implante _ -> 
    comment := (!comment) @ [ "Non_implantee" ]; (!note, !comment, true)
  | _ -> 
    comment := (!comment) @ [ "Test_non_complete" ]; (!note, !comment, true);;


(* --------------------------------------------------------------------------- *)
(* -- TESTE TOUT ------------------------------------------------------------- *)
(* --------------------------------------------------------------------------- *)

let test() =
  let all_tests = [ "includeSep", 8., test1;
                    "height", 12., test2;
                    "ptree2stree", 20., test3;
                    "tree2mtree", 40., test4;
                    "mtree2pretty", 20., test5
                  ] in
  List.fold_left 
    (fun (l_res,erreur) (nom_f,bareme,t) -> 
       let (note, comment, err) = t () in
         (l_res @ [(nom_f, bareme, note, comment, err)], erreur || err)
    ) ([],false) all_tests;;
