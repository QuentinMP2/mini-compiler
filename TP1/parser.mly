%{
 (* type interface = Auto of string | Iface of string *)
%}


%token <string> ID
%token AUTO
%token IFACE
%token INET
%token EOF
%token IP 
%token LOOPBACK
%token DHCP
%token ADDRESS
%token GATEWAY
%token NETMASK
%token STATIC 

(* Exercice 2 *)
(* Déclarations du type de l'attribut associé à un non terminal *)
(* Dans un premier temps on ignore cet attribut -> type unit *)
%type <string list * string list> i
%type <unit> t

(* Indication de l'axiom et du type de l'attribut associé à l'axiom *)
(* Dans un premier temps on ignore cet attribut -> type unit *)
%start <string list * string list> is

%%

is :
| couple_liste = i couples_listes = is {let (a,b), (c,d) = couple_liste, couples_listes in (a@c, b@d)} (* action sémantique associée à une règle de prodution -> dans un premier temps () *)
| EOF  {([],[])}

i : 
| AUTO id_inter = ID {([], [id_inter])}
| IFACE id_inter = ID INET t {([id_inter], [])}

t : 
| LOOPBACK {()}
| DHCP {()}
| STATIC ADDRESS IP NETMASK IP GATEWAY IP {()}

%%
