{
open Parser
open Printf
exception Eof
exception LexingError of string

let lineno = ref 1
let linestart = ref (-1)

let newline lexbuf : unit =
  linestart := Lexing.lexeme_start lexbuf;
  incr lineno

let info lexbuf =
  let c1 = Lexing.lexeme_start lexbuf in
  let c2 = Lexing.lexeme_end lexbuf in
  let l = !lineno in
  let c = !linestart + 1 in
    ((l, c1 - c),(l, c2 - c - 1))

let error lexbuf msg =
  let i = info lexbuf in
  let t = Lexing.lexeme lexbuf in
  let ((l1,c1),(l2,c2)) = i in
  let s =
    if l2=l1
    then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
    else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2 in
  let err = Printf.sprintf "%s: lexing error %s at %s."
    s
    msg
    t in
  raise (LexingError err)
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let lletter = ['a'-'z']
let uletter = ['A'-'Z']
let word = (uletter | lletter) (uletter | lletter | digit | '_')*
let symbol = ('!' | ['#'-'~'])+
let string = '"' (word | digit | white | symbol)+ '"'

rule read = 
  parse
  | string { CONTENT (Lexing.lexeme lexbuf) }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "settings" { SETTINGS }
  | "pagestyle" { PAGESTYLE }
  | "letter" { LETTER }
  | "A4" { A4 }
  | "pageorient" { PAGEORIENT }
  | "portrait" { PORTRAIT }
  | "landscape" { LANDSCAPE }
  | "fontstyle" { FONTSTYLE }
  | "times" { TIMES } 
  | "arial" { ARIAL }
  | "cambria" { CAMBRIA }
  | "fontsize" { FONTSIZE }
  | "linespacing" { SPACING }
  | "single" { SINGLE }
  | "onepointfive" { ONEPOINTFIVE }
  | "double" { DOUBLE }
  | "marginsize" { MARGINSIZE }
  | "text" { TEXT }
  | "equation" { EQUATION }
  | "table" { TABLE }
  | ";" { SEMI }
  | ":" { COLON }
  | white { read lexbuf }
  | eof { EOF }