(* -------------------------------------------------------------------------- *)
(* ----------------------- TP1 - IFT-3000 - Hiver 2018 ---------------------- *)
(* -------------------------------------------------------------------------- *)

(******************************************************************************)
(* Spécification                                                              *)
(******************************************************************************)
module type PTREE = 
  sig
    (* Utilisée par le testeur et correcteur du Tp *)
    exception Non_Implante of string

    (* Principales structures de données du Tp *)
    type ('formula, 'rule) pTree =
        PF of 'formula
      | PT of 'rule * 'formula * ('formula, 'rule) pTree list

    type strTree =
        St of int
      | Leaf of string
      | Tree of string * string * strTree list

   (* Signatures des fonctions su Tp à compléter *)
    val includeSep : string -> string list -> string
    val height : strTree -> int
    val ptree2stree : ('formula -> string) -> ('rule -> string) -> 
                      ('formula, 'rule) pTree -> strTree
    val tree2mtree : ?l:int -> strTree -> (int * strTree) list 
    val mtree2pretty : (strTree -> string) -> (int -> string) -> string -> 
                       (int * strTree) list -> string list

   (* Signatures d'autres fonctions du Tp *)
    val id2str : int -> string
    val tree2str : strTree -> string
    val id2latex : int -> string
    val tree2latex : strTree -> string
 
    val genstree : 
      ?tree2str:(strTree -> string) -> 
      ?id2str:(int -> string) -> strTree -> unit
   
    val genltree :
      ?l:int ->
      ?tree2latex:(strTree -> string) ->
      ?id2latex:(int -> string) -> strTree -> int
 
end

