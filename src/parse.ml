let rec parse_villes cin nb_villes =
  if nb_villes = 0
  then []
  else
    let v = Scanf.fscanf cin "%s %f %f " (fun na x y -> na,x,y) in
    v::parse_villes cin (nb_villes -1)


let parse_input cin =
  let nb_villes = Scanf.fscanf cin "%d " (fun x -> x) in
  List.rev (parse_villes cin nb_villes)

exception Syntax_error of string*string  

let parse_param cin =
        let construct = Scanf.fscanf cin "%s " (fun x -> x) in
        let remplissage = Scanf.fscanf cin "%s " (fun x -> x) in 
        let opti = Scanf.fscanf cin "%s " (fun x -> x) in
        if (construct = "ONE" || construct = "HULL")
        && (remplissage = "RANDOM" || remplissage = "NEAREST" || remplissage = "FARTHEST")
        && (opti = "REPOSITIONNEMENT" || opti = "INVERSION") then
                construct, remplissage, opti
        else
                raise (Syntax_error("Paramètres invalides","\n"))


exception File_not_found of string

let parse_input_file file_name =
  try
    let cin = open_in file_name in
    try
      let l = parse_input cin in
      let _ = close_in cin in  (* parce qu'on est pas des porcs *)
      l
    with e -> close_in cin;raise e
  with
  | Sys_error _ -> (* fichier non trouvé *)
     raise (File_not_found file_name)
  | Scanf.Scan_failure msg ->
     raise (Syntax_error(file_name,msg))

let parse_param_file file_name =
  try
    let cin = open_in file_name in
    try
      let c, r, o = parse_param cin in
      let _ = close_in cin in  (* parce qu'on est pas des porcs *)
      c, r, o
    with e -> close_in cin;raise e
  with
  | Sys_error _ -> (* fichier non trouvé *)
     raise (File_not_found file_name)
  | Scanf.Scan_failure msg ->
     raise (Syntax_error(file_name,msg))
      
