(* Definition du module de vs *)

module Ville:Map.OrderedType with type t = int 

module S : Map.S with type key = Ville.t

module type Carte = 
        sig
                (* type enregistrement qui contient *)
                type coord

                (* le type répresentant une carte *)
                type carte

                (* une carte ne contenant aucune ville *)
                val empty : carte

                (* @requires : rien
                 * @ensures : renvoie true si la carte est vide, false sinon
                 * @raises : rien*)
                val is_empty : carte -> bool

                (* @requires : rien
                 * @ensures : renvoie le cardinal de la carte
                 * @raises : rien*)
                val cardinal : carte -> int
                
                (* @requires : rien
                 * @ensures : construit une coordonnée depuis des valeurs
                 * @raises : rien*)
                val c_coord : string -> bool -> float -> float -> coord

                val add : S.key -> coord -> carte -> carte

                val lex_add : coord -> carte -> carte

                val remove : S.key -> carte -> carte

                val find : S.key -> carte -> coord
                
                val get_xy : S.key -> carte -> (float*float)

                val coord_to_tuple : coord -> (string*bool*(float*float))

                val get_inserted : S.key -> carte -> bool

                val inserted : coord -> bool

                val distance : S.key -> S.key -> carte -> float

                val mark : S.key -> carte -> carte

                val fold : (S.key -> coord -> 'b -> 'b) -> carte -> 'b -> 'b

                val print : carte -> unit

                val plot : carte -> unit
        end


module CarteComplete : Carte 

module type Chemin =
        sig 
                module C : Carte

                type chemin

                (* fonctions de base *)

                val empty : chemin

                val is_empty : chemin -> bool

                val find : S.key-> chemin -> (S.key*S.key)

                (* params *)
                val insert : S.key -> S.key -> chemin -> chemin

                val remove : S.key -> chemin -> chemin

                val fold : (S.key -> (S.key * S.key) -> 'b -> 'b ) -> chemin -> 'b -> 'b

                val distance : S.key -> S.key -> chemin -> C.carte -> float

                val insert_best_spot : S.key -> chemin -> C.carte -> chemin

                val one : C.carte -> (chemin * C.carte)

                val enveloppe_convexe : C.carte -> (chemin * C.carte)
                (* fonctions d'insertion dans le chemin *)

                val rand_build : C.carte -> chemin -> chemin

                val best_build : C.carte -> chemin -> chemin
                
                val worst_build : C.carte -> chemin -> chemin
                
                val replace : S.key -> chemin -> C.carte -> chemin

                val local_invert : S.key -> chemin -> C.carte -> chemin

                val optimize : (S.key -> chemin -> C.carte -> chemin) -> chemin -> C.carte -> chemin

                val print : chemin -> C.carte -> unit

                val plot : chemin -> C.carte -> unit
        end

module FaitChemin ( X : Carte ) : Chemin with module C = X
(*module CarteInc : Carte =
        struct 

        end
*)

