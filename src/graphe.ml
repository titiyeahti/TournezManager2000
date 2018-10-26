(* Definition du module de vs *)

module type Carte =
        sig
                module S : Map

                type ville

                type coordonnee
                
                (* le type effectif d'une carte *)
                type carte

                val distance : ville -> ville -> carte -> float

                val empty : carte

                val is_empty : carte -> bool

                val add : ville -> coordonnee -> carte -> carte

                val remove : ville -> carte -> carte
        end

module type Chemin = 
        sig
                type carte

                type ville

                type chemin

                val empty : chemin

                val is_empty : chemin

                (* la ville à insérer, son prédécesseur et son successeur ainsi que le chemin à modifier *)
                val inserer : ville -> ville -> ville -> chemin -> chemin

                val remove : ville -> chemin -> chemin
                
                (* il s'agit de la distance entre deux villes dans le chemin : 
                        * soit c le chemin v1 - v2 - ... - vn - v1 alors distance vi vj = min (\sum_(n=i)^(j-1) distance vn vn+1), ... *)
                val distance : ville -> ville -> float
        end

module MakeCarte(X:Map.OrderedType) : Carte =
        struct
                module S = Map.Make(X)

                type ville = X.t 

                type coordonnee = (float*float)

                type carte = coordonnee S.t

                let distance v1 v2 g = 
                        try 
                                match S.find v1 g with 
                                x1,y1 -> match S.find v2 g with 
                                x2,y2 -> ((x1-.x2)*.(x1-.x2)+.(y1-.y2)*.(y1-.y2))**0.5
                        with Not_found -> -1.
                
                let empty = S.empty

                let is_empty c = c = empty

                let add v xy c = S.add v xy c

                let remove v c = S.remove v c
        end

module MakeChemin(X:Carte) : Chemin =
        struct 

                type carte = X.carte

                type ville = X.ville

                (* Map indexée par les villes et contenant les villes précédentes et suivantes *)
                type chemin = (ville*ville) X.S.t

                let empty c = X.empty

                let is_empty c = c = empty

                let inserer v vp vs c = 
