open Chemin

module Ch = FaitChemin(CarteComplete)

let insert_random_ville ca = 
        CarteComplete.lex_add (CarteComplete.c_coord (string_of_int (Random.int 100000))
        false (Random.float 800.) (Random.float 900.)) ca

let rec map_random i =
        if i = 0 then CarteComplete.empty
        else insert_random_ville (map_random (i-1)) 


(* test *)

let _ = Random.self_init ()

let t1 = Sys.time ()

let _ = Printf.printf "début : carte \n"

let m = map_random 1000

(* let _ = CarteComplete.print m*)

(*let rb = Ch.rand_build m

let bb = Ch.best_build m*)

let t2 = Sys.time ()

let _ = Printf.printf "carte =  %f \n" (t2-.t1)

let wb = Ch.rand_build m

let t3 = Sys.time ()

let _ = Printf.printf "chemin = %f \n" (t3-.t2)

let wbopt = Ch.optimize Ch.replace wb m

let t4 = Sys.time ()

let wbopt2 = Ch.optimize Ch.local_invert wb m

let t5 = Sys.time ()

let _  = Printf.printf "opti1 = %f \n opti2 = %f \n total = %f\n" (t4-.t3) (t5-.t4) t5 
(*
let _ = Printf.printf "\n de manière arbitraire \n"

let _ = Printf.printf "distance totale = %f \n" (Ch.distance 1 1 rb  m)

let _ = Printf.printf "\n de meilleure manière \n"

let _ = Printf.printf "distance totale = %f \n" (Ch.distance 1 1 bb  m) *)

let _ = Printf.printf "\nde pire manière \n"

let _ = Printf.printf "distance totale = %f \n" (Ch.distance 1 1 wb  m)

let _ = Printf.printf "le pire après optimization1 D = %f \n " (Ch.distance 1 1 wbopt m)

let _ = Printf.printf "le pire après optimization2 D = %f \n " (Ch.distance 1 1 wbopt2 m)

(*
open Graphics

let _ = open_graph " 800x900"

let _ = set_color (rgb 255 0 0)

let _ = CarteComplete.plot m 

let _ = set_color (rgb 0 0 255)

let _ = Ch.plot wb m

let _ = input_line(stdin)

let _ = set_color (rgb 0 255 0)

let _ = Ch.plot wbopt

let _ = input_line stdin*)
