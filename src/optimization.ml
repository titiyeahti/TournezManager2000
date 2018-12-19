open Chemin

let _ = Random.self_init ()

let insert_random_ville ville carte = 
        let coord = IChCa.c_coord "" false (Random.float 100.) (Random.float 100.) in
        IChCa.ca_add ville coord carte

let rec map_random i =
        if Ville.compare i 0 = 0 then IChCa.ca_empty 
        else insert_random_ville (i:IChCa.ville) (map_random (i-1))

let m = map_random 10

let _ = IChCa.ca_print m

let first_way carte = 
        IChCa.ca_fold (fun v coord chemin -> 
                IChCa.ch_inserer v 1 chemin) carte IChCa.ch_empty

let fw = first_way m

let _ = IChCa.ch_print fw

let _ = Printf.printf "distance totale = %f \n" (IChCa.ch_distance 1 1 fw m)


