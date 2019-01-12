open Chemin

module Ch = FaitChemin(CarteComplete)

let argv = Sys.argv 

let c, r, o = Parse.parse_param_file (Array.get argv 1)

let l = Parse.parse_input_file (Array.get argv 2)

let rec carte_from_list l = match l with
    | [] -> CarteComplete.empty
    | h::t -> let nom, x, y = h in 
        CarteComplete.lex_add (CarteComplete.c_coord nom false x y) (carte_from_list t)

let _ = Graphics.open_graph " 100x100"

let main =
        let carte = carte_from_list l in
        let chemin, carte_2 = if c = "HULL" then 
                Ch.enveloppe_convexe carte
        else
                Ch.one carte
        in
        let _ = Printf.printf "1\n" in
        let chemin_rempli = (*let _ = Ch.plot chemin carte in *)
        if r = "RANDOM" then 
                Ch.rand_build carte_2 chemin
        else if r = "NEAREST" then
                Ch.best_build carte_2 chemin
        else 
                Ch.worst_build carte_2 chemin
        in
        let _ = Printf.printf "1\n" in
        let _ = Graphics.set_color (Graphics.rgb 255 0 0) in
        let chemin_optimise = let _ = Ch.plot chemin_rempli carte in
        if o = "INVERSION" then 
                Ch.optimize Ch.local_invert chemin_rempli carte
        else 
                Ch.optimize Ch.replace chemin_rempli carte
        in
        let _ = Printf.printf "1\n" in
        let _ = Graphics.set_color (Graphics.rgb 0 255 0) in

        Ch.plot chemin_optimise carte

let _ = input_line stdin



