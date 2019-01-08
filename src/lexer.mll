(* Parser *)
{
        open Parser
        exception Eof
}
rule token = parse
          [' ''\t'] { token lexbuf }
        | ['0'-'9']* as str { INT(int_of_string(str)) }
        | '\n' { EOL }
        | '<'(['A'-'Z''a'-'z']* as str)'>' { NOM(str) }
        | '<'['0'-'9']*'.'['0'-'9']*'>' as str { FLOAT(float_of_string(str)) }
