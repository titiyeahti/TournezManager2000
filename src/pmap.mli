(*
 * Implémentation de l'interface décrite dans le fichier ensemble.mli
 * Imporation via 'open Ensemble'
 *)


module type Ordered =
    sig
        type t;;

        val compare : t -> t -> int;;

    end


module type Map =
    sig 

        exception EmptySet
        
        exception KeyNotFound

        type elt
        
        type 'a map
        
        val empty : 'a map
        
        val is_empty : 'a map -> bool
            
        val mem : elt -> 'a -> 'a map -> bool

        val mem_key : elt -> 'a map -> bool

        val get_elt_from_key : elt -> 'a map -> 'a
        
        val add : elt -> 'a -> 'a map -> 'a map

        val set : elt -> 'a -> 'a map -> 'a map
        
        val fold : (elt -> 'a -> 'a) -> 'a map -> 'a -> 'a
        
        val get_min : 'a map -> (elt*'a)
        
        val equal : 'a map -> 'a map -> bool
        
        val remove : elt -> 'a map -> 'a map                 
        
        val union : 'a map -> 'a map -> 'a map
    
    end

module Entier:Ordered

module AvlMap(X:Ordered):Map 
