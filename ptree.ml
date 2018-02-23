(* -------------------------------------------------------------------------- *)
(* ----------------------- TP1 - IFT-3000 - Hiver 2018 ---------------------- *)
(* -------------------------------------------------------------------------- *)
(* Matricule étudiant: 111125564                                              *)
(* -------------------------------------------------------------------------- *)
(* -- PRINCIPALE FICHIER DU TP: FONCTIONS À COMPLÉTER ----------------------- *)
(* -------------------------------------------------------------------------- *)

#use "ptree.mli";;


(******************************************************************************)
(* Implantation                                                               *)
(******************************************************************************)
module PTree : PTREE = struct

  (* Utilisée par le testeur et correcteur du Tp *)
  exception Non_Implante of string

(* Principales structures de données du TP ---------------------------------- *)
  type ('formula, 'rule) pTree = 
      PF of 'formula 
    | PT of 'rule * 'formula * ('formula, 'rule) pTree list

  type strTree = 
      St of int
    | Leaf of string
    | Tree of string * string * strTree list

(* -------------------------------------------------------------------------- *)
(* Début partie code (implantation) à compléter ----------------------------- *)
(* -------------------------------------------------------------------------- *)
  open List

  (* -- À IMPLANTER/COMPLÉTER (8 PTS) --------------------------------------- *)
  (* @Fonction      : includeSep : string -> string list -> string            *)
  (* @Description   : retourne une liste d'elts en format string, séparés par
                      une valeur passée en argument                           *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : aucune                                                  *)
  let rec includeSep sep l =
    match l with
    | [] -> sep
    | e::r -> sep ^ e ^ (includeSep sep r)
  ;;
    

  (* -- À IMPLANTER/COMPLÉTER (12 PTS) -------------------------------------- *)
  (* @Fonction      : height : strTree -> int                                 *)
  (* @Description   : retourne la taille d'un arbre passé en argument         *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : la valeur retournée est positive et correspond à taille *)
  let rec height t =
    match t with
    | Leaf(_) | St(_) -> 0
    | Tree(_, _, []) -> 1
    | Tree(l, r, hdSubTree::tlSubTree) -> max (1 + height hdSubTree) (height (Tree(l, r, tlSubTree)))
  ;;


  (* -- À IMPLANTER/COMPLÉTER (20 PTS) -------------------------------------- *)
  (* @Fonction      : ptree2stree : ('formula -> string)-> ('rule -> string) -> 
                                    ('formula, 'rule) pTree -> strTree        *)
  (* @Description   : traduit un arbre prrofTree en un arbre strTree          *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : l'arbre retourné est correct                            *)
  let rec ptree2stree formula2str rule2str pt =
    match pt with
    | PF(formula) -> Leaf(formula2str formula)
    | PT(rule, formula, treeList) -> Tree(rule2str rule, formula2str formula,
        map (fun x -> ptree2stree formula2str rule2str x) treeList)
  ;;

  let getTreeReferencePair number t = 
    match t with
    | St(_) -> (t, []);
    | Leaf(_) -> (t, []);
    | Tree(a,b,c) -> (St(number), [t])
  ;;

  let rec foldStrTree postNodeTraitement f v0 t =

    match t with
    | Tree(a, b, c) -> postNodeTraitement (fold_left (fun r t' -> foldStrTree postNodeTraitement f r t') (f v0 t) c)
    | _ -> f v0 t
  ;;

  let getTreeAsTreeWithSubTreeAsRef maxh currentNumber t =   
    match t with
    | Tree(a, b, c) -> Tree(a, b, fold_left (fun acc t' -> 
        if height t' <= maxh then
          acc@[t']
        else
          acc@[((fun (r, _) -> r) (getTreeReferencePair (length acc + currentNumber) t'))]
      ) [] c)
    | _ -> t
  ;;

  let moveFirstElementToEnd l =
    match l with
    | [] -> []
    | x::r -> r@[x]
  ;;

  let sortPairByFirstElement l =
    sort (fun (first, _) (second, _) -> compare first second) l
  ;;

  let ruleOnLeaf acc newTree =
  (
    Printf.printf "  ruleOnLeaf\n";
    match acc with
    | [] -> 
      (
        Printf.printf "    acc: []\n";
        []
      )
    | (n, x)::r ->
      (
        Printf.printf "    acc: (n, x)::r\n";
        match x with
        | Tree(a', b', c') -> (n, Tree(a', b', c'@[newTree]))::r
        | _ -> [] (*Ne devrait pas avoir de non-arbre dans cette liste.*)
      )
  )

  let ruleOnNode acc a b =
  (
    Printf.printf "  ruleOnNode\n";
    match acc with 
    | [] -> 
      (
        Printf.printf "    acc: []\n";
        [(1, Tree(a, b, []))]
      )
    | (n, x)::r -> 
      (
        Printf.printf "    acc: (n, x)::r %d\n" n;
        match x with 
        | Tree(a', b', c') -> (length acc + 1, Tree(a, b, []))::(n, Tree(a', b', c'@[St(length acc + 1)]))::r
        | _ -> [] (*Ne devrait pas avoir de non-arbre dans cette liste.*)
      )
  )

  let ruleOnEmptyNode acc newTree =
  (
    Printf.printf "  ruleOnEmptyNode\n";
    match acc with
    | [] -> 
      (
        Printf.printf "    acc: []\n";
        [(1, newTree)]
      )
    | (n, x)::r ->
      (
        Printf.printf "    acc: (n, x)::r\n";
        match x with
        | Tree(a', b', c') -> (n, Tree(a', b', c'@[St(length acc + 1)]))::r@[(length acc + 1, newTree)]
        | _ -> [] (*Ne devrait pas avoir de non-arbre dans cette liste.*)
      )
  )


  (* -- À IMPLANTER/COMPLÉTER (40 PTS) -------------------------------------- *)
  (* @Fonction      : tree2mtree : ?level:int->strTree->(int * strTree) list  *)
  (* @Description   : transforme un arbre en liste de sous-arbres             *)
  (* @Precondition  : level doit être positive ou nulle                       *)
  (* @Postcondition : les arbres retournées sont correctement liées           *)
  let tree2mtree ?(l=0) t =
  let splitTree = 
    foldStrTree moveFirstElementToEnd (fun acc t' -> 
      Printf.printf "Matching height:%d acc:%d\n" (height t') (length acc);
      if (l = 0) || (length acc = 0) || (height t' > l) then
      (
        Printf.printf "  (l = 0) || (nInAcc = 1) || (height t' > l)\n";
        match t' with
        | Tree(a, b, []) -> ruleOnEmptyNode acc t'
        | Tree(a, b, c) -> ruleOnNode acc a b
        | _ -> ruleOnLeaf acc t'
      )
      else
      (
        if height t' = l then
        (
          match t' with
          | Tree(a,b,c) -> ruleOnEmptyNode acc t'
          | _ -> ruleOnLeaf acc t'
        )
        else
          acc
      )

    )
  in
    sortPairByFirstElement (splitTree [] t)
  ;;






    (*match t with
    | St(_) -> []
    | Leaf(_) -> []
    | Tree (a, b, c) -> fold_left(fun accPair tree -> 
      if height tree > l then
        (*Subdiviser*)
        (length accPair + 1, Tree(a, b, 
          fold_left(fun accToSt tree2 ->
            match tree2 with
            | St(_) -> tree2::accToSt
            | Leaf(_) -> tree2::accToSt
            | Tree(_) -> accToSt@[St(length accToSt + length accPair + 1)]
        ) [] c ))::accPair
      else
        (length accPair + 1, tree)::accPair
    ) [] [t]
  ;;*)


  (* -- À IMPLANTER/COMPLÉTER (20 PTS) -------------------------------------- *)
  (* @Fonction      : mtree2pretty : (strTree -> string) -> (int -> string) -> 
                                     string->(int * strTree) list->string list*)
  (* @Description   : retourne une liste de chaines de caractères correspondant
                      à l'arbre, ou aux arbres, à afficher                    *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : aucune                                                  *)
  let mtree2pretty tree2pr id2pr sep tl = 
    raise (Non_Implante "«mtree2pretty» à compléter")

(* -------------------------------------------------------------------------- *)
(* Fin partie code (implantation) à compléter ------------------------------- *)
(* -------------------------------------------------------------------------- *)


(* Module comprenant des fonctions utiles pour la génération en Latex ------- *)
  module Utiles = struct

    (* read_lines_file : string -> string list *)
    let read_lines_file file =
      let rec read_lines f l =
        try 
          read_lines f (l @ [input_line f])
        with End_of_file -> l
      in
      if Sys.file_exists file then
        let ic = open_in file in
        let l = read_lines ic [] in
        let _ = close_in ic in
        l
      else
        failwith ("Fichier <" ^ file ^ "> introuvable!")

    (* mix_files : string -> string list -> string -> string -> unit *)
    let mix_files header latex footer dest =
      let l1 = read_lines_file header 
      and l2 = read_lines_file footer 
      and out_chan = open_out dest in
      List.iter (fun s -> output_string out_chan (s^"\n")) (l1@latex@l2);
      close_out out_chan

  end

  open Utiles

 (* Génération en Texte ------------------------------------------------------ *)
  let id2str n =
    "A" ^ (string_of_int n) ^ ":"

  let tree2str t = 
    match t with
    | Tree(r, down, up_ltree) ->
      let up_ltree' = 
        map (fun t -> match t with  
                      | Leaf s -> s 
                      | St n -> "A" ^ (string_of_int n)
                      | _ -> failwith "Impossible de convertir!"
            ) up_ltree in
      let up_str = includeSep "   " up_ltree' in
      let n1 = String.length up_str in
      let n2 = String.length down in
      let n = max n1 n2 in
      let sepline = String.make n '-' in
      let indent = String.make ((n - n2) / 2) ' ' in
      "\t" ^ up_str ^ "\n\t" ^ sepline ^ " " ^ r ^ "\n\t" ^ indent ^ down
    | _ -> failwith "Ne correspond pas à un arbre!"


  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : genstree : (strTree -> string) -> (int -> string) ->    *)
  (*                             strTree -> unit                              *)
  (* @Description   : affiche un arbre en mode texte (terminal)               *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : aucune                                                  *)
  let genstree ?(tree2str=tree2str) ?(id2str=id2str) t =
    let lt = mtree2pretty tree2str id2str "\n" (tree2mtree ~l:0 t) in 
    iter print_endline lt

(* Génération en Latex ------------------------------------------------------ *)
  let id2latex n =
    "A_{" ^ (string_of_int n) ^ "}: \\ & "

  let rec tree2latex t = 
    match t with
    | St n -> "A_{" ^ (string_of_int n) ^ "}"
    | Leaf s -> s 
    | Tree(r, down, up_ltree) ->
      let up_ltree' = 
        if up_ltree = [] then "\\Box"
        else
           fold_left 
               (fun acc t -> 
                  let res = tree2latex t  in
                  acc ^ res ^ "~~~~~"
               ) "~~~~~" up_ltree 
      in
      "\\cfrac{" ^ up_ltree' ^ "}{" ^ down ^ "}" ^ r

  (* ------------------------------------------------------------------------ *)
  (* @Fonction      : genltree : (strTree -> string) -> (int -> string) ->    *)
  (*                             strTree -> int                               *)
  (* @Description   : affiche un arbre en mode Latex dans un navigateur       *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition : aucune                                                  *)
  let genltree ?(l=0) ?(tree2latex=tree2latex) ?(id2latex=id2latex) t =
    let lt = mtree2pretty tree2latex id2latex "\\\\\\\\" (tree2mtree ~l:l t) 
    in 
    let _ = mix_files "header.html" lt "footer.html" "tp1-h18.html" in
    match Sys.os_type with
    | "Win32" -> Sys.command ("start tp1-h18.html")
    | _ -> Sys.command ("xdg-open tp1-h18.html")

end