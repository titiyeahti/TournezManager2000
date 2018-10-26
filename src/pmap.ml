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

type ('a,'b) avl = Void | Node of 'a * 'b * ('a,'b) avl * ('a,'b) avl * int;;

module Entier =
    struct
        
        type t = int;;
        
        let compare x y = x-y;;
    
    end


module AvlMap(X:Ordered) = 
    struct
        
        exception EmptySet;;

        exception KeyNotFound;;

        type elt = X.t;;

        type 'a map = (X.t,'a) avl;;

        let empty = Void;;

        let is_empty avl = avl = empty;;

        let rec mem a b = function 
            |Void -> false
            |Node(k, j, g, d, _) -> let c = compare a k in 
                if c = 0 then j = b 
                else if c < 0 then mem a b g
                else mem a b d
        ;;

        let rec mem_key a = function 
            |Void -> false
            |Node(k, j, g, d, _) -> let c = compare a k in 
                if c = 0 then true 
                else if c < 0 then mem_key a g
                else mem_key a d
        ;;

        let rec get_elt_from_key key = function 
            |Void -> raise KeyNotFound
            |Node(a, b, g, d, _) -> let c = compare key a in 
                if c = 0 then  b
                else if c < 0 then get_elt_from_key key g
                else get_elt_from_key key d
        ;;

        let hauteur = function
            |Void -> 0
            |Node(a, _, g, d, h) -> h
        ;;

        let node a b g d =
            let m = max (hauteur d) (hauteur g) in 
            Node(a, b, g, d, m+1)
        ;;

        let balance = function
            |Void -> Void
            |Node(a, b, g, d, _) -> 
                    let hg = hauteur g in 
                    let hd = hauteur d in 
                    if hg > hd + 1 then
                        match g with 
                        |Void -> assert false (*plop, ce cas est impossible*)
                        |Node(ag, bg, gg, gd, _) -> 
                                let hgg = hauteur gg in 
                                let hgd = hauteur gd in 
                                if hgg > hgd then 
                                    node ag bg gg (node a b gd d)
                                else
                                    match gd with 
                                        |Void -> assert false
                                        |Node(agd, bgd, gdg, gdd, _) ->
                                                node agd bgd (node ag bg gg gdg) (node a b gdd d)
                    else if hd > hg + 1 then
                        match d with
                        |Void -> assert false 
                        |Node(ad, bd, dg, dd, _) ->
                                let hdg = hauteur dg in
                                let hdd = hauteur dd in
                                if hdd > hdg then 
                                    node ad bd (node a b g dg) dd
                                else
                                    match dg with
                                        |Void -> assert false
                                        |Node(adg, bdg, dgg, dgd, _) -> 
                                                node adg bdg (node a b g dgg) (node ad bd dgd dd) 
                    else 
                        node a b d g
        ;;

        let rec add a b = function 
            |Void -> node a b empty empty
            |Node(k, j, g, d, _) -> let c = compare a k in 
                if c = 0 then node k j g d
                else if c < 0 then balance(node k j (add a b g) d)
                else balance(node k j g (add a b d))
        ;;

        let rec fold f abr v0 = match abr with
        |Void -> v0
        |Node(k, j, g, d, _) -> fold f d (f k (fold f g v0))
        ;;

        let rec get_min = function 
            |Void -> raise EmptySet
            |Node(k, j, g, d, _) -> 
                if g = Void then k,j 
                else get_min g
        ;;

        let rec inclue a = function 
            |Void -> true
            |Node(k, j, g, d, _) -> mem k j a && inclue a g && inclue a d
        ;;

        let equal a1 a2  = inclue a1 a2 && inclue a2 a1
        ;;

        let rec remove a = function 
            |Void -> raise EmptySet
            |Node(k, l, g, d, _) -> let c = compare a k in
                if c < 0 then balance(node k l (remove a g) d)
                else if c > 0 then balance(node k l g (remove a d))
                else match d with 
                |Void -> g
                |Node(j, _, g2, d2, _) -> let m1, m2 = get_min(d) in 
                    balance(node m1 m2 g (remove m1 d))
        ;;

        let rec union a = function 
            |Void -> a
            |Node(k, j, g, d, _) -> let b = add k j a in 
                union (union b g) d
        ;;

    end







