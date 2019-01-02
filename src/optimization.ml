open Chemin

module Ch = FaitChemin(CarteComplete)

let insert_random_ville ville carte = 
                let coord = CarteComplete.c_coord "" false (Random.float 100.) (Random.float 100.) in
                CarteComplete.add ville coord carte

let rec map_random i =
        if i = 0 then CarteComplete.empty 
        else insert_random_ville i (map_random (i-1))



(* test *)

let _ = Random.self_init ()

let m = map_random 100

let _ = CarteComplete.print m

let first_way carte = 
        CarteComplete.fold (fun v coord chemin -> 
                Ch.insert v 1 chemin) carte Ch.empty

let fw = first_way m

let _ = Ch.print fw

let _ = Printf.printf "distance totale = %f \n" (Ch.distance 1 1 fw m)


