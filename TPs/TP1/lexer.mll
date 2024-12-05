{
(* type token = ID of string | IP | AUTO | IFACE | INET | LOOPBACK | DHCP | ADDRESS | GATEWAY | NETMASK | STATIC | EOF *)
open Parser

exception Error of string
}

(* Définitions de macro pour les expressions régulières *)
let blanc = [' ' '\t' '\n']
let nom_interface = ['a'-'z']['a'-'z''0'-'9''_']*
let nb = ['0'-'9']|('1'['0'-'9']['0'-'9'])|('2'(['0'-'4']['0'-'9']|('5'['0'-'5'])))
let adresses_IP = nb '.' nb '.' nb '.' nb


(* Règles léxicales *)
rule interface = parse
|  blanc (* On ignore les blancs *)
    { interface lexbuf }
| "auto"
    { AUTO }
| "iface"
    { IFACE }
| "inet"
    { INET }
| "loopback"
    { LOOPBACK }
| "dhcp"
    { DHCP }
| "address"
    { ADDRESS }
| "gateway"
    { GATEWAY }
| "netmask"
    { NETMASK }
| "static"
    { STATIC }
| nom_interface as name    
    { ID name }
| adresses_IP
    { IP }
| eof
    { EOF }
| _
{ raise (Error ("Unexpected char: "^(Lexing.lexeme lexbuf)^" at "^(string_of_int (Lexing.lexeme_start
lexbuf))^"-"^(string_of_int (Lexing.lexeme_end lexbuf)))) }
