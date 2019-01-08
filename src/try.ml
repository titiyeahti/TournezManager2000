open Chemin

module Ch = FaitChemin(CarteComplete)

let insert_random_ville ca = 
        CarteComplete.lex_add (CarteComplete.c_coord (string_of_int (Random.int 100000))
        false (Random.float 100.) (Random.float 100.)) ca

let rec map_random i =
        if i = 0 then CarteComplete.empty
        else insert_random_ville (map_random (i-1)) 


(* test *)

let _ = Random.self_init ()

let m = map_random 20

let _ = CarteComplete.print m

let rb = Ch.rand_build m

let bb = Ch.best_build m

let wb = Ch.worst_build m

let _ = Printf.printf "\n de manière arbitraire \n"

let _ = Ch.print rb 

let _ = Printf.printf "distance totale = %f \n" (Ch.distance 1 1 rb  m)

let _ = Printf.printf "\n de meilleure manière \n"

let _ = Ch.print bb 

let _ = Printf.printf "distance totale = %f \n" (Ch.distance 1 1 bb  m)

let _ = Printf.printf "\n de pire manière \n"

let _ = Ch.print wb

let _ = Printf.printf "distance totale = %f \n" (Ch.distance 1 1 wb  m)

