%{
%}

%token <float> FLOAT
%token <string> NOM
%token <int> INT
%token EOL
%start main
%type <Chemin.CarteComplete.t> main
%type <Chemin.CarteComplete.coord> ville 
%%

main :
        | INT  { Chemin.CarteComplete.empty } 
        | ville main { Chemin.CarteComplete.lex_add $1 $2 }
;

ville : 
        NOM FLOAT FLOAT EOL { Chemin.CarteComplete.c_coord $1 false $2 $3 }
;



