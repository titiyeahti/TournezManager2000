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

        val cardinal : carte -> int

        val c_coord : string -> bool -> float -> float -> coord

        val add : S.key -> coord -> carte -> carte

        val lex_add : coord -> carte -> carte

        val remove : S.key -> carte -> carte

        val find : S.key -> carte -> coord

        val close_road : S.key -> S.key -> carte -> carte

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

        let cardinal c = S.cardinal c

        let c_coord n b x y = {nom = n; i = b; c = x,y}

        let add v co ca = S.add v co ca

        let lex_add co ca = let v = S.cardinal ca in add v co ca

        let remove v ca = S.remove v ca

        let find v ca = S.find v ca

        let close_road a b ca = ca

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
                if not c0.i then 
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

module CarteInc : Carte =
struct
        type coord = {nom : string ; i : bool; c : (float*float); rb : int S.t}

        type carte = coord S.t

        let empty = S.empty

        let is_empty c = c = empty

        let cardinal c = S.cardinal c

        let c_coord n b x y = {nom = n; i = b; c = x,y; rb = S.empty}

        let add v co ca = S.add v co ca

        let lex_add co ca = let v = S.cardinal ca in add v co ca

        let remove v ca = S.remove v ca

        let find v ca = S.find v ca

        let close_road a b ca = 
                let coa = find a ca in
                let cob = find b ca in
                let ncoa = {nom = coa.nom; i = coa.i; c = coa.c; rb = S.add b 0 coa.rb} in
                let ncob = {nom = cob.nom; i = cob.i; c = cob.c; rb = S.add a 0 cob.rb} in
                add a ncoa (add b ncob ca)

        let get_xy v ca = (find v ca).c

        let coord_to_tuple co = co.nom, co.i, co.c

        let get_inserted v ca = (find v ca).i

        let inserted co = co.i

        let distance v1 v2 ca =
                let c1 = find v1 ca in
                let c2 = find v2 ca in
                let b = S.mem v2 c1.rb in
                if b then -1.
                else
                        let x1, y1 = c1.c in
                        let x2, y2 = c2.c in
                        ((x1-.x2)**2. +. (y1-.y2)**2.)**0.5

        let mark v ca = 
                let c0 = find v ca in
                if not c0.i then 
                        let c1 = {nom = c0.nom; i = true; c = c0.c; rb = c0.rb} in
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

        val insert_best_spot : S.key -> chemin -> C.carte -> chemin

        val one : C.carte -> (chemin * C.carte)

        val enveloppe_convexe : C.carte -> (chemin * C.carte)

        (* pas d'ordre aléatoire juste un ordre quelconque *)
        val rand_build : C.carte -> chemin -> chemin

        val best_build : C.carte -> chemin -> chemin

        val worst_build : C.carte -> chemin -> chemin 

        val replace : S.key -> chemin -> C.carte -> chemin

        val local_invert : S.key -> chemin -> C.carte -> chemin

        val optimize : (S.key -> chemin -> C.carte -> chemin) -> chemin -> C.carte -> chemin

        val print : chemin -> C.carte -> unit

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

        let insert_best_spot v ch ca =
                if ch = empty then 
                        insert v (-1) ch
                else
                        let a1, ps1 = S.min_binding ch in
                        let _, b1 = ps1 in
                        let d1 = (C.distance a1 v ca) +. (C.distance v b1 ca) -. (C.distance a1 b1 ca) in
                        let spot, _ = fold (fun current (p,s) (best, best_dist) ->
                                let dist = (C.distance current v ca) +. (C.distance v s ca) -. (C.distance current s ca) in
                                if dist < best_dist then 
                                        current, dist
                                else
                                        best, best_dist) ch (a1, d1)
                                        in insert v spot ch

        (* cvx enveloppe *)

        let rec left_cvx p q (ch,ca) =
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
                        ch, ca
                else
                        let new_ca = C.mark h ca in
                        left_cvx p h (left_cvx h q ((insert h p ch), new_ca))

        let print_debug ch = S.iter (fun v (p,s) -> Printf.printf "%i -> (%i, %i)\n" v p s) ch 

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
                                ) ca (-1, 0.0) in 
                let new_ca = C.mark p (C.mark q ca) in
                left_cvx q p (left_cvx p q (insert q p (insert p p empty), new_ca))
        
        let one ca = 
                let _ = Random.self_init () in
                let v = Random.int (C.cardinal ca) in 
                (insert v (-1) empty), (C.mark v ca)

        (* construction *)

        let rand_build ca ch_init =
                let ch, _ = C.fold (fun v co (cur_ch, cur_ca) ->
                        if not (C.inserted co) then 
                                insert_best_spot v cur_ch cur_ca, C.mark v cur_ca
                        else (cur_ch, cur_ca)) ca (ch_init, ca)
                in ch

        let dist_segment v a b ca =
                let xv, yv = C.get_xy v ca in
                let xa, ya = C.get_xy a ca in
                let xb, yb = C.get_xy b ca in
                let ab = C.distance a b ca in
                let av = C.distance a v ca in
                let vb = C.distance v b ca in
                let ah = ((xv-.xa)*.(xb-.xa) +. (xv-.xa)*.(xb-.xa))/.ab in
                if av > -1. && vb > -1. then 
                        if ah > 0.0 && ah < ab then 
                                ((av*.av)-.(ah*.ah))**0.5
                        else
                                av
                else
                        -1.

        let dist_ensemble v ch ca =
                if ch = empty then 0.0, v
                else
                        fold (fun a (p,b) (best, spot) ->
                                let dist = (*C.distance p a ca*)dist_segment v a b ca in
                                if dist > -1. then 
                                        if spot = -1 || dist < best then 
                                                dist, a
                                        else
                                                best, spot
                                else
                                        best, spot
                        ) ch (-1., -1)

        let plus_proche_ville v ca = 
                C.fold (fun cur co (best_dist, plus_proche) ->
                        let dist = C.distance cur v ca in
                        if cur <> v && (dist < best_dist || plus_proche = -1) && dist > -1. then
                                dist, cur
                        else
                                best_dist, plus_proche
                ) ca (-1., -1)

        let list_dist ca = 
                C.fold (fun v co l -> 
                        if not (C.inserted co) then
                                let dist, spot = plus_proche_ville v ca in
                                if dist > -1. then 
                                        (v, (dist, spot))::l
                                else
                                        l
                        else
                                l
                ) ca []

        let get_farest ca ch l =
                let bv, (bd, bs) =
                List.fold_right (fun (v, (dist, spot)) (best_ville, (best_dist, best_spot)) ->
                        let cv = C.find v ca in
                        let cspot = C.find spot ca in
                        if (not (C.inserted cv)) &&  (dist > best_dist || best_ville = -1) then
                                 if (C.inserted cspot) then (v, (dist, spot))
                                 else 
                                         let d, s = dist_ensemble v ch ca in
                                         if d > best_dist then 
                                                 v, (d, s)
                                         else
                                                (best_ville, (best_dist, best_spot))
                        else
                                (best_ville, (best_dist, best_spot)))
                l (-1, (0.0, -1)) in
                bv, bs

        let get_nearest ca ch l =
                let bv, (bd, bs) =
                List.fold_right (fun (v, (dist, spot)) (best_ville, (best_dist, best_spot)) ->
                        let cv = C.find v ca in
                        let cspot = C.find spot ca in
                        if (not (C.inserted cv)) &&  (dist < best_dist || best_ville = -1) && dist > -1. then
                                 if (C.inserted cspot) then (v, (dist, spot))
                                 else 
                                         let d, s = dist_ensemble v ch ca in
                                         if d < best_dist || best_ville = -1 then 
                                                 v, (d, s)
                                         else
                                                (best_ville, (best_dist, best_spot))
                        else
                                (best_ville, (best_dist, best_spot)))
                l (-1, (-1., -1)) in
                bv, bs

        let best_build ca_init ch_init = 
                let l = list_dist ca_init in
                let rec aux ca ch =
                        let v, spot = get_nearest ca ch l in
                        if v = -1 then
                                ch, ca
                        else
                                aux (C.mark v ca) (insert_best_spot v ch ca) 
                        in
                let res, _ = aux ca_init ch_init in res

        let worst_build ca_init ch_init = 
                let l = list_dist ca_init in
                let rec aux ca ch =
                        let v, spot = get_farest ca ch l in
                        if v = -1 then 
                                ch, ca
                        else
                                aux (C.mark v ca) (insert_best_spot v ch ca) 
                        in
                let res, _ = aux ca_init ch_init in res

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

        let rec print_aux cur dest ch ca = 
                let n, _, _ = C.coord_to_tuple (C.find cur ca) in 
                let p, s = find cur ch in  
                let _ = Printf.printf "%s " n in
                if p = dest then 
                        let n2, _, _ = C.coord_to_tuple (C.find p ca) in
                        Printf.printf "%s\n" n2                        
                else 
                        print_aux p dest ch ca

        let print ch ca =
                let dist = distance 0 0 ch ca in 
                let _ = Printf.printf "%f : " dist in
                print_aux 0 0 ch ca

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
