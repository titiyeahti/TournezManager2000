module Ville:Map.OrderedType with type t = int 

(* module qui va servir de base aux types Chemin et Carte*)
module S : Map.S with type key = Ville.t

(* module qui permet de faire le lien entre une ville identifiée par un entier et ses informations*)
module type Carte = 
        sig
                (* type enregistrement qui contient notament les coordonné de la ville 
                 * ainsi que son nom*)
                type coord

                (* le type répresentant une carte 
                 * il s'agit d'un ensemble de couples (ville.t * coord) 
                 * (je risque de l'appeler ville par la suite) *)
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
                
                (* @requires : rien
                 * @ensures : la carte (add v co ca) 
                        contient une entrée pour v de valeur co
                 * @raises : rien*)
                val add : S.key -> coord -> carte -> carte
                
                (* @requires : rien
                 * @ensures :  la carte (lex_add co ca) 
                        contient une entrée pour (cardinal ca) de valeur co
                 * @raises : rien*)
                val lex_add : coord -> carte -> carte

                (* @requires : rien
                 * @ensures : (remove v ca) ne contient pas d'entrée pour ca
                 * @raises : rien*)
                val remove : S.key -> carte -> carte

                (* @requires : ca doit contenir v
                 * @ensures : (find v ca) donne l'element coord assicié a v
                 * @raises : Not_found si v n'est pas dans ca *)
                val find : S.key -> carte -> coord

                val close_road : S.key -> S.key -> carte -> carte
               
                (* @requires : ca doit contenir v
                 * @ensures : (get_xy v ca) donne les coordonnées de v
                 * @raises : Not_found si v n'est pas dans ca *)
                val get_xy : S.key -> carte -> (float*float)

                (* @requires : rien
                 * @ensures : (coord_to_tuple co) met les données du type coord dans un tuple
                 * @raises : rien*)
                val coord_to_tuple : coord -> (string*bool*(float*float))
 
                (* @requires : ca doit contenir v
                 * @ensures : (get_inserted v ca) donne valeur du booléen qui renseigne sur l'insertion de v (initialisé à false)
                 * @raises : Not_found si v n'est pas dans ca *)
                val get_inserted : S.key -> carte -> bool

                (* @requires : rien
                 * @ensures : (inserted co) donne la valeur du booléen contenu dans coord
                 * @raises : Not_found si v n'est pas dans ca *)
                val inserted : coord -> bool

                (* @requires : a et b doivent etre dans ca
                 * @ensures : (distance a b ca) vaut la distance euclidienne de a à b dans c si a et b sont connectés, -1 sinon
                 * @raises : Not_found si a ou b n'est pas dans ca *)
                val distance : S.key -> S.key -> carte -> float

                (* @requires : v doit être dans ca
                 * @ensures : (mark v ca) retourne une copie de v ca dans laquelle le booléen de v vaut true
                 * @raises : Not_found si v n'est pas dans ca *)
                val mark : S.key -> carte -> carte
                
                (* @requires : rien
                 * @ensures : fold f ca v0 calcule (f kN cN ... (f k1 c1 v0) ...) vaut la distance euclidienne de a à b dans c
                 * @raises : rien *)
                val fold : (S.key -> coord -> 'b -> 'b) -> carte -> 'b -> 'b

                val print : carte -> unit

                val plot : carte -> unit
        end

(* module carte dans lequel toutes les villes sont connectées
 * implémenté avec des Map *)
module CarteComplete : Carte 


module type Chemin =
        sig 
                (* un module qui permet d'utilier les fonction de Carte dans Chemin*)
                module C : Carte

                (* type dans lequel sont assocciés les villes avec leurs prédecesseurs et leurs succeseurs *) 
                type chemin

                (* fonctions de base *)

                (* fonctionnement similaire aux fonction eponymes de Carte *)
                val empty : chemin

                val is_empty : chemin -> bool

                val find : S.key-> chemin -> (S.key*S.key)

                (* @requires : (insert v p ch)
                 * si ch = empty alors rien 
                 * si cardinal ch = 1 alors p doit etre dans ch
                 * sinon ch doit etre correct (une seule boucle) et p doit être dans ch
                 * @ensures : (insert v p ch) est un chemin correct ou v est entre p et l'ancien successeur de p
                 * @raises : Not_found*)
                val insert : S.key -> S.key -> chemin -> chemin

                (* @requires : chemin bien construit
                 * @ensures : remove v ch est toujours bien contruit
                 * @raises : rien *)
                val remove : S.key -> chemin -> chemin

                (* cf Map.fold *)
                val fold : (S.key -> (S.key * S.key) -> 'b -> 'b ) -> chemin -> 'b -> 'b

                (* @requires : (distance a b ch ca) les villes de ch doivent être dans ca, le chemin doit être bien construit, a et b doivent être dans ch
                 * @ensures : (distance a b ch ca) calcule la distance de a à b dans le chemin
                 * @raises : Not_found, Stack_Overflow*)
                val distance : S.key -> S.key -> chemin -> C.carte -> float

                (* @requires : (insert_best_spot v ch ca) les villes de ch doivent être dans ca, ch doit être bien construit, v doit être dans ch
                 * @ensures : (insert_best_spot v ch ca) est ch dans lequel on a inséré v de manière à minimiser l'augmentation de distance
                 * @raises : Not_found*)
                val insert_best_spot : S.key -> chemin -> C.carte -> chemin

                (* @requires : pour toute ville v de ca, 0 <= v <= (cardinal ca -1)
                 * @ensures : (nch, nca) = (one ca), nch ne contient qu'une seule ville, et cette ville est marquée dans nca 
                 * @raises : rien*)
                val one : C.carte -> (chemin * C.carte)

                (* @requires : (enveloppe_convexe ca) cardinal ca >= 2
                 * @ensures : (nch, nca) = (enveloppe_convexe ca), nch est le chemin correspondant à l'enveloppe convexe de ca, toutes les villes de nch sont marquées dans nca
                 * @raises : rien*)
                val enveloppe_convexe : C.carte -> (chemin * C.carte)
                (* fonctions d'insertion dans le chemin *)

                (* @requires : (rand_build ca ch) le chemin et la carte doivent être valide, deplus, les ville de ch doivent êtres marquées dans ca
                 * @ensures : (rand_build ca ch) toutes les villes de ca sont dans nch et nch est bien construit
                 * @raises : Not_found*)
                val rand_build : C.carte -> chemin -> chemin

                (* @requires : (best_build ca ch) le chemin et la carte doivent être valide, deplus, les ville de ch doivent êtres marquées dans ca
                 * @ensures : (best_build ca ch) toutes les villes de ca sont dans nch et nch est bien construit
                 * @raises : Not_found*)
                val best_build : C.carte -> chemin -> chemin
               
                (* @requires : (worst_build ca ch) le chemin et la carte doivent être valide, deplus, les ville de ch doivent êtres marquées dans ca
                 * @ensures : (worst_build ca ch) toutes les villes de ca sont dans nch et nch est bien construit
                 * @raises : Not_found*)
                val worst_build : C.carte -> chemin -> chemin
                
                (* @requires : ch valide, ca correspondant à ch et v dans ch
                 * @ensures : (replace v ca ch) est presque identique à ch à la différence que v à été replacé au meilleur endroit possible
                 * @raises : Not_found *)
                val replace : S.key -> chemin -> C.carte -> chemin

                (* @requires : ch valide, ca correspondant à ch et v dans ch
                 * @ensures : (replace v ca ch) est presque identique à ch à la différence qu'on a procédé à une inversion locale avec v et le meilleur candidat possible
                 * @raises : Not_found *)
                val local_invert : S.key -> chemin -> C.carte -> chemin

                (* @requires : ch valide, ca correspondant à ch et v dans ch
                 * @ensures : longeur totale de (optimize f ca ch) <= longueur de ch
                 * @raises : Not_found *)
                val optimize : (S.key -> chemin -> C.carte -> chemin) -> chemin -> C.carte -> chemin

                val print : chemin -> C.carte -> unit

                val plot : chemin -> C.carte -> unit
        end

module FaitChemin ( X : Carte ) : Chemin with module C = X

module CarteInc : Carte

