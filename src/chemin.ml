(* Definition du module de vs *)

module Ville:Map.OrderedType with type t = int =
        struct 
                type t = int

                let compare v w = v-w 
        end

module S = Map.Make(Ville)

module type Carte = 
        sig
                (* un type enregistrement contenant les coordonnées d'une S.key ainsi que d'autres informations *)
                type coord

                (* le type répresentant une carte *)
                type carte

                (* une carte ne contenant aucune S.key *)
                val empty : carte

                val is_empty : carte -> bool

                val c_coord : string -> bool -> float -> float -> coord

                val add : S.key -> coord -> carte -> carte

                val remove : S.key -> carte -> carte

                val find : S.key -> carte -> coord

                val distance : S.key -> S.key -> carte -> float

                val mark : S.key -> carte -> carte

                val fold : (S.key -> coord -> 'b -> 'b) -> carte -> 'b -> 'b

                val print : carte -> unit
        end


module CarteComplete : Carte =
        struct
                type coord = {nom : string ; i : bool; c : (float*float)}

                type carte = coord S.t

                let empty = S.empty

                let is_empty c = c = empty

                let c_coord n b x y = {nom = n; i = b; c = x,y}

                let add v co ca = S.add v co ca

                let remove v ca = S.remove v ca

                let find v ca = S.find v ca

                let distance v1 v2 ca = 
                        let x1, y1 = (find v1 ca).c in
                        let x2, y2 = (find v2 ca).c in
                        ((x1-.x2)**2. +. (y1-.y2)**2.)**0.5

                let mark v ca = 
                        let c0 = find v ca in
                        if c0.i=false then 
                                let c1 = {nom = c0.nom; i = true; c = c0.c} in
                                add v c1 ca
                        else
                                ca

                let fold f ca v0 = 
                        S.fold f ca v0

                let print ca =
                    S.iter (fun ville coord -> match coord.c with 
                                x, y -> Printf.printf 
                                "%s    id : %i   coord : %f, %f\n" 
                                coord.nom ville x y) ca
        end

module type Chemin =
        sig 
                module C : Carte

                type chemin

                val empty : chemin

                val is_empty : chemin -> bool

                val find : S.key-> chemin -> (S.key*S.key)

                (* params *)
                val insert : S.key -> S.key -> chemin -> chemin

                val remove : S.key -> chemin -> chemin

                val distance : S.key -> S.key -> chemin -> C.carte -> float

                val fold : (S.key -> (S.key * S.key) -> 'b -> 'b ) -> chemin -> 'b -> 'b 

                val print : chemin -> unit
        end

module FaitChemin ( X : Carte ) : Chemin with module C = X =
        struct
                module C = X

                type chemin = (S.key * S.key) S.t

                let empty = S.empty

                let is_empty c = c = empty

                let find v c =
                        S.find v c
                
                let insert v vp ch = 
                        (* chemin vide *)
                        if (is_empty ch) then  S.add v (v, v) ch
                        else 
                                let vpp, vps = S.find vp ch in 
                                (* chemin avec une seule ville *)
                                if vp = vpp then
                                        S.add v (vp, vp) (S.add vp (v,v) ch)
                                (* sinon *)
                                else  
                                        let vpsp, vpss = S.find vps ch in
                                        S.add v (vp, vps) (S.add vp (vpp, v) (S.add vps (v, vpss) ch))

                let remove v ch = 
                        try
                                let vp, vs = S.find v ch in
                                (* une seule ville *)
                                if vp = v then 
                                        (S.remove v ch)

                                (* 2 villes *)
                                else 
                                        if vp=vs then
                                                S.add vp (vp, vp) (S.remove v ch)
                                else
                                        let vpp, vps = S.find vp ch in 
                                        let vsp, vss = S.find vs ch in 
                                        S.add vp (vpp, vs) (S.add vs (vp, vss) (S.remove v ch))
                        with Not_found -> ch

                let rec distance v1 v2 ch ca = 
                        let v1p, v1s = find v1 ch in 
                        if v1s = v2 then 
                                (C.distance v1 v2 ca) 
                        else
                                (C.distance v1 v1s ca) +. distance v1s v2 ch ca
                
                let fold f ch v0 = S.fold f ch v0

                let print ch = 
                        S.iter (fun ville ps -> 
                                let p, s = ps in 
                                Printf.printf 
                                "id : %i    preced : %i     suiv : %i\n" 
                                ville p s) ch
        end

(*module CarteInc : Carte =
        struct 

        end
*)

