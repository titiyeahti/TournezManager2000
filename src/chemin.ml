(* Definition du module de vs *)

module type Carte =
        sig
                exception Phantom_city
                
                module Y:Map.OrderedType

                type ville = Y.t

                type coordonnee
                
                (* le type effectif d'une carte *)
                type carte

                val distance : ville -> ville -> carte -> float

                val empty : carte

                val is_empty : carte -> bool

                val add : ville -> coordonnee -> carte -> carte

                (*modifie change le booléen coord.i identifié par la clef ville si elle existe*)
                val insert : ville -> carte -> carte

                val remove : ville -> carte -> carte
        end

module type Chemin = 
        sig
                type carte

                type ville

                type chemin

                val empty : chemin

                val is_empty : chemin -> bool

                (* la ville à insérer, son prédécesseur et son successeur ainsi que le chemin à modifier *)
                val inserer : ville -> ville -> ville -> chemin -> chemin

                val remove : ville -> chemin -> chemin
                
                (* il s'agit de la distance entre deux villes dans le chemin : 
                        * soit c le chemin v1 - v2 - ... - vn - v1 alors distance vi vj = min (\sum_(n=i)^(j-1) distance vn vn+1), ...
                        * de plus distance *)
                val distance : ville -> ville -> chemin -> carte -> float
        end


module MakeCarte(X:Map.OrderedType) : Carte =
        struct
                exception Phantom_city

                module Y = X

                module S = Map.Make(X)

                type ville = X.t 

                type coordonnee = {nom : string ; i : bool; c : (float*float)}

                type carte = (coordonnee) S.t

                let distance v1 v2 g = 
                        try 
                                match S.find v1 g with 
                                c1 -> match S.find v2 g with 
                                c2 -> match c1.c with 
                                    x1,y1 -> match c2.c with
                                    x2,y2 -> ((x1-.x2)*.(x1-.x2)+.(y1-.y2)*.(y1-.y2))**0.5
                        with Not_found -> raise Phantom_city
                
                let empty = S.empty

                let is_empty c = c = empty

                let to_coord n b x y = {nom = n; i = b; c = x,y}

                let add v coord c = S.add v coord c

                let add_clean v n b x y c = add v (to_coord n b x y) c

                (* met à jour la carte pour savoir que l'on l'a insérer *)
                let insert v c = 
                        try let coord = S.find v c in
                            if coord.i then c
                            else match coord.c with x, y -> add_clean v coord.nom true x y c 
                        with Not_found -> raise Phantom_city

                let remove v c = S.remove v c
        end

module MakeChemin(X:Carte) : Chemin =
        struct 

                module S = Map.Make(X.Y)

                type carte = X.carte

                type ville = X.ville

                (* Map indexée par les villes et contenant les villes précédentes et suivantes *)
                type chemin = (ville*ville) S.t

                let empty = S.empty

                let is_empty c = c = empty

                let inserer v vp vs = S.add v (vp,vs)

                let remove v ch = S.remove v ch

                let rec distance v1 v2 ch carte = 
                       let v1p, v1s = S.find v1 ch in 
                       let d = X.distance v1 v1s carte in
                       if v1s = v2 then d
                       else d +. (distance v1s v2 ch carte)
        
        end

module Ville:Map.OrderedType =
        struct 
                type t = int

                let compare v w = v-w 
        end

module VraieCarte = MakeCarte(Ville)

module VraiChemin = MakeChemin(VraieCarte)


