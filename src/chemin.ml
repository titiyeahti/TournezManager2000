(* Definition du module de vs *)

module Ville:Map.OrderedType with type t = int =
        struct 
                type t = int

                let compare v w = v-w 
        end


module type CheminCarte =
    sig
        exception Phantom_city

        module S:Map.S

        type ville

        type coordonnee

        (* le type effectif d'une carte *)
        type carte

        type chemin

        val ca_distance : ville -> ville -> carte -> float

        val c_coord : string -> bool -> float -> float -> coordonnee

        val ca_empty : carte

        val ca_is_empty : carte -> bool

        val ca_add : ville -> coordonnee -> carte -> carte

        val ca_find : ville -> carte -> coordonnee

        (*modifie change le booléen coord.i identifié par la clef ville si elle existe*)
        val ca_insert : ville -> carte -> carte

        val ca_remove : ville -> carte -> carte

        val ca_fold : (ville -> coordonnee -> 'b -> 'b) -> carte -> 'b -> 'b

        val ca_print : carte -> unit

        val ch_empty : chemin

        val ch_is_empty : chemin -> bool

        (* la ville à insérer, son prédécesseur et son successeur ainsi que le chemin à modifier *)
        val ch_inserer : ville -> ville -> chemin -> chemin

        val ch_find : ville -> chemin -> (ville*ville)

        val ch_remove : ville -> chemin -> chemin

        (* il s'agit de la distance entre deux villes dans le chemin : 
            * soit c le chemin v1 - v2 - ... - vn - v1 alors distance vi vj = min (\sum_(n=i)^(j-1) distance vn vn+1), ...
            * de plus distance *)
        val ch_distance : ville -> ville -> chemin -> carte -> float

        val ch_fold : (ville -> (ville*ville) -> 'b -> 'b) -> chemin -> 'b -> 'b  
       
        val ch_print : chemin -> unit
    end


module IChCa : CheminCarte with type ville = Ville.t =
    struct
        exception Phantom_city

        module S = Map.Make(Ville)

        type ville = Ville.t

        type coordonnee = {nom : string ; i : bool; c : (float*float)}

        type carte = (coordonnee) S.t
        (* Map indexée par les villes et contenant les villes précédentes et suivantes *)
        type chemin = (ville*ville) S.t

        let ca_distance v1 v2 g = 
            try 
                    match S.find v1 g with 
        c1 -> match S.find v2 g with 
        c2 -> match c1.c with 
            x1,y1 -> match c2.c with
            x2,y2 -> ((x1-.x2)*.(x1-.x2)+.(y1-.y2)*.(y1-.y2))**0.5
            with Not_found -> raise Phantom_city

        let c_coord n inserted x y =
            {nom = n; i = inserted; c = x,y}

        let ca_empty = S.empty

        let ca_is_empty c = c = ca_empty

        let to_coord n b x y = {nom = n; i = b; c = x,y}

        let ca_add v coord c = S.add v coord c

        let ca_find v c = S.find v c

        let add_clean v n b x y c = ca_add v (to_coord n b x y) c

        (* met à jour la carte pour savoir que l'on l'a insérer *)
        let ca_insert v c = 
            try let coord = S.find v c in
            if coord.i then c
        else match coord.c with x, y -> add_clean v coord.nom true x y c 
        with Not_found -> raise Phantom_city

        let ca_remove v c = S.remove v c

        let ca_fold f c v0 = S.fold f c v0

        let ca_print c =  
            S.iter (fun ville coord -> match coord.c with 
            x, y -> Printf.printf 
        "%s    id : %i   coord : %f, %f\n" coord.nom ville x y) c
    

        let ch_empty = S.empty

        let ch_is_empty c = c = ch_empty

        let ch_inserer v vp ch = 
                if ch = ch_empty then  S.add v (v, v) ch
                else 
                    try 
                    let vpp, vps = S.find vp ch in 
                    if vp = vpp then
                        S.add v (vp, vp) (S.add vp (v,v) ch)
                    else  
                        let vpsp, vpss = S.find vps ch in
                        S.add v (vp, vps) (S.add vp (vpp, v) (S.add vps (v, vpss) ch))
                    with Not_found -> raise Not_found

        let ch_find v c = S.find v c

        let ch_remove v ch = 
            try
                let vp, vs = S.find v ch in
                if vp = v then 
                  (S.remove v ch)
                else 
                  if vp=vs then
                    S.add vp (vp, vp) (S.remove v ch)
                  else
                    let vpp, vps = S.find vp ch in 
                    let vsp, vss = S.find vs ch in 
                    S.add vp (vpp, vs) (S.add vs (vp, vss) (S.remove v ch))
            with Not_found -> ch

        let rec ch_distance v1 v2 ch carte = 
            let v1p, v1s = S.find v1 ch in 
            let d = (ca_distance v1 v1s carte) in
            if v1s = v2 then d
            else d +. (ch_distance v1s v2 ch carte)

        let ch_fold f c v0 = S.fold f c v0

        let ch_print c = 
            S.iter (fun ville ps -> match ps with 
            p, s -> Printf.printf 
            "id : %i    preced : %i     suiv : %i\n" ville p s) c
    end

module type Chemin =
        sig 

        end

module type Carte = 
        sig

        end

module CarteComplete : Carte =
        struct

        end

module CarteInc : Carte =
        struct 

        end

module FaitChemin ( C : Carte ) : Chemin =
        struct

        end


