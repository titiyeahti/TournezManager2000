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

module CarteComplete : Carte =
struct
        type coord = {nom : string ; i : bool; c : (float*float)}

        type carte = coord S.t

        let empty = S.empty

        let is_empty c = c = empty

        let c_coord n b x y = {nom = n; i = b; c = x,y}

        let add v co ca = S.add v co ca

        let lex_add co ca = let v = S.cardinal ca in add v co ca

        let remove v ca = S.remove v ca

        let find v ca = S.find v ca

        let get_xy v ca = (find v ca).c

        let coord_to_tuple co = co.nom, co.i, co.c

        let get_inserted v ca = (find v ca).i

        let inserted co = co.i

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

        let plot ca = 
                S.iter (fun v co ->
                        let x, y = co.c in 
                        Graphics.plot (int_of_float x) (int_of_float y)) ca
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

        val fold : (S.key -> (S.key * S.key) -> 'b -> 'b ) -> chemin -> 'b -> 'b

        val distance : S.key -> S.key -> chemin -> C.carte -> float

        val dist_ensemble : S.key -> chemin -> C.carte -> float

        val insert_best_spot : S.key -> chemin -> C.carte -> chemin

        val enveloppe_convexe : C.carte -> chemin

        (* pas d'ordre aléatoire juste un ordre quelconque *)
        val rand_build : C.carte -> chemin -> chemin

        val best_build : C.carte -> chemin -> chemin

        val worst_build : C.carte -> chemin -> chemin 

        val replace : S.key -> chemin -> C.carte -> chemin

        val local_invert : S.key -> chemin -> C.carte -> chemin

        val optimize : (S.key -> chemin -> C.carte -> chemin) -> chemin -> C.carte -> chemin

        val print : chemin -> unit

        val plot : chemin -> C.carte -> unit 
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

        let fold f ch v0 = S.fold f ch v0

        let rec distance v1 v2 ch ca = 
                let v1p, v1s = find v1 ch in 
                if v1s = v2 then 
                        (C.distance v1 v2 ca) 
                                else
                                        (C.distance v1 v1s ca) +. distance v1s v2 ch ca

        let dist_ensemble v ch ca =
                if ch = empty then 0.0
                else
                        let d0 = 
                                let v0, ps = S.choose ch in
                                C.distance v0 v ca
                                in
                fold (fun vch ps best ->
                        let _, s  = ps in
                        let xvch, yvch = C.get_xy vch ca in
                        let xs, ys = C.get_xy s ca in
                        let xv, yv = C.get_xy v ca in
                        (* distance entre les points vch et v *)
                        let vch_v = C.distance vch v ca in
                        (* distance orientée de vch à la base de la hauteur dans (vch v s)*)
                        let vch_h = ((xv-.xvch)*.(xs-.xvch) +. (yv-.yvch)*.(ys-.yvch))/.
                            (C.distance vch s ca) in
                        if (vch_h < 0.0 || vch_v < vch_h)  then (min best vch_v)
                        else (min best (vch_v**2. -. vch_h**2.)**0.5)
                        ) ch d0

                        let insert_best_spot v ch ca =
                                if ch = empty then 
                                        insert v (-1) ch
                        else
                                let a1, ps1 = S.min_binding ch in
                                let _, b1 = ps1 in
                                let d1 = (C.distance a1 v ca) +. (C.distance v b1 ca) 
                                -. (C.distance a1 b1 ca) in
                                let spot, _ = fold (fun current (p,s) (best, best_dist) ->
                                        let dist = (C.distance current v ca) +. 
                                (C.distance v s ca) -. (C.distance current s ca) in
                                        if dist < best_dist then 
                                                current, dist
                                else
                                        best, best_dist) ch (a1, d1)
                                        in insert v spot ch

                                        ;;

        (* cvx enveloppe *)

        let rec left_cvx p q ca ch =
                let xp, yp = C.get_xy p ca in
                let xq, yq = C.get_xy q ca in 
                let pq = C.distance p q ca in
                let cos_alpha = (xq-.xp)/.pq in
                let sin_alpha = (yq-.yp)/.pq in 
                let h, proj = C.fold (fun current co (best, proj_min) ->
                        (* on va projetter PCurrent sur n, la normale à PQ telle que 
                         * si (Ox, PQ) = aplha alors (Ox, n) = alpha - pi/2
                         * le point de projeté minimal correspondra au point le plus à 
                         * gauche de PQ si le projeté est négatif.
                         *
                         * n = (sin alpha, -cos aplha) *)
                        if (current <> p && current <> q) then
                                let _, _, (xc, yc) = C.coord_to_tuple co in
                                let current_proj = (xc-.xp) *. sin_alpha -. (yc-.yp) *. cos_alpha in
                                if current_proj <= proj_min then 
                                        current, current_proj
                                else
                                        best, proj_min
                        else
                                best, proj_min) ca (-1, 0.0) in
                if h = -1 then
                        ch
                else
                        left_cvx p h ca (left_cvx h q ca (insert h p ch))

        let enveloppe_convexe ca =
                (* point d'abcisse minimale *)
                let p, pco = C.fold (fun v co (best,x_best) ->
                        let _, _, (x, y) = C.coord_to_tuple co in 
                        if x < x_best || best = -1 then 
                                v, x
                        else
                                (best, x_best)
                        ) ca (-1, 0.0) in
                let q, qco = C.fold (fun v co (best,x_best) ->
                        let _, _, (x, y) = C.coord_to_tuple co in 
                        if x > x_best || best = -1 then 
                                v, x
                        else
                                (best, x_best)
                                ) ca (-1, 0.0)
                        in
                        let _ = Printf.printf "on est presque à la récursion\n P, Q = %i, %i\n" p, q in
                left_cvx q p ca (left_cvx p q ca (insert q p (insert p p empty)))


        (* construction *)

        let rand_build ca ch =
                C.fold (fun v coord acc -> 
                        insert_best_spot v acc ca) ca empty

        let get_farest ca ch = 
                let v, d = C.fold (fun vi co (bv, bd) ->
                        if not (C.inserted co) then 
                                let nd = dist_ensemble vi ch ca in
                                if (bv = -1 || nd > bd) then 
                                        vi, nd
                        else
                                bv, bd
                                else
                                        bv, bd
                                ) ca (-1,0.0) 
                                in v

        let get_nearest ca ch = 
                let v, d = C.fold (fun vi co (bv, bd) ->
                        if not (C.inserted co) then 
                                let nd = dist_ensemble vi ch ca in
                                if (bv = -1 || nd < bd) then 
                                        vi, nd
                        else
                                bv, bd
                                else
                                        bv, bd
                                ) ca (-1,0.0) 
                                in v

        let best_build ca ch = 
                let rec aux ca ch =
                        let v = get_nearest ca ch in
                        if v = -1 then 
                                ch
                        else
                                aux (C.mark v ca) (insert_best_spot v ch ca) 
                        in
                aux ca empty

        let worst_build ca ch = 
                let rec aux ca ch =
                        let v = get_farest ca ch in
                        if v = -1 then 
                                ch
                        else
                                aux (C.mark v ca) (insert_best_spot v ch ca) 
                        in
                aux ca empty

        (* optimization *)

        let replace v ch ca = 
                let p, s = find v ch in 
                (* il existe un chemin entre les deux points *)
                if C.distance p s ca >= 0.0 then
                        let ch1 = remove v ch in
                        let newch = insert_best_spot v ch1 ca in
                        if (distance v v newch ca) < (distance v v ch ca) then 
                                newch
                        else
                                ch
                        else
                                ch

        let optimize opt ch ca = 
                C.fold (fun v co acc -> opt v acc ca) ca ch

        let print ch = 
                S.iter (fun ville ps -> 
                        let p, s = ps in 
                        Printf.printf 
                        "id : %i    preced : %i     suiv : %i\n" 
                        ville p s) ch

        let rec swap_sequence sb pc ch =
                (*let _ = Printf.printf "coucou\n" in*)
                let psb, ssb = find sb ch in
                if sb = pc then
                        S.add sb (ssb, psb) ch
                else
                        S.add sb (ssb, psb) (swap_sequence ssb pc (S.add sb (ssb, psb)ch))

        let local_invert a ch ca =
                let v0 = -1, 0.0 in
                let pa, b = find a ch in
                let spot, _ = fold (fun c (_, sc) (best, best_diff) ->
                        let _, d = find c ch in
                        if c = a || d = a || b = c then 
                                (best, best_diff)
                else
                        let ac = C.distance a c ca in 
                        let db = C.distance b d ca in
                        let ab = C.distance a b ca in
                        let cd = C.distance c d ca in 
                        let diff =  ab +. cd -. ac -. db in
                        if diff > best_diff then
                                c, diff
                        else
                                best, best_diff
                                ) ch v0 in
                if spot = -1 then 
                        ch
                        else 
                                let pc, d = find spot ch in
                                let _, sb = find b ch in
                                let _, sd = find d ch in 
                                let ch_temp = 
                                        S.add a (pa, spot) 
                                        (S.add d (b, sd) 
                                        (S.add spot (a, pc)
                                        (S.add b (sb, d)
                                        ch))) in
                                if sb = spot then 
                                        ch_temp 
                else 
                        swap_sequence sb pc ch_temp 


        let plot ch ca =
                let v0, ps0 = S.min_binding ch in
                let p0, s0 = ps0 in
                let x, y = C.get_xy  v0 ca in
                let _ = Graphics.moveto (int_of_float x) (int_of_float y) in
                let rec aux target current =
                        let xc, yc = C.get_xy current ca in
                        let _ = Graphics.lineto (int_of_float xc) (int_of_float yc) in
                        if target <> current then 
                                let _, s = find current ch in
                                aux target s
                                in aux p0 v0
end

                                        (*module CarteInc : Carte =
                                                struct 

                                        end
*)

