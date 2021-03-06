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

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+ ('.'? digit+)*
let lletter = ['a'-'z']
let uletter = ['A'-'Z']
let word = (uletter | lletter) (uletter | lletter | digit | '_')*
let symbol = ('!' | ['#'-'~'])+
let string = '"' (word | digit | white | symbol)+ '"'

rule read = 
  parse
  | white { read lexbuf }
  | '\n' { newline lexbuf; read lexbuf }
  | int { let i = Lexing.lexeme lexbuf in INT (i) }
  | string { let s = Lexing.lexeme lexbuf in CONTENT (String.sub s 1 ((String.length s) - 2)) }
  | "{" { LCURLY }
  | "}" { RCURLY }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "." { PERIOD }
  | "<<" { LANGLE }
  | ">>" { RANGLE }
  | "_sig''" { SIGMADOUBLEPRIME }
  | "_sig'" { SIGMAPRIME }
  | "_sig" { SIGMA }
  | "_lam" { LAM }
  | "_Lam" { BIGLAM }
  | "_tau" { TAU }
  | "_tau'" { TAUPRIME }
  | "_tau0" { TAUZERO }
  | "_tau1" { TAUONE }
  | "_tau2" { TAUTWO }
  | "_Gamma" { GAMMA }
  | "_Delta" { DELTA }
  | "_forall" { FORALL }
  | "->" { SMALL }
  | "=>" { BIG }
  | "->*" { MULTI }
  | "-/>" { NOTSMALL }
  | "=/>" { NOTBIG }
  | "-/>*" { NOTMULTI }
  | "!" { FACT }
  | "not" { NOT }
  | "+" { PLUS }
  | "*" { MULT }
  | "-" { MINUS }
  | "/" { DIV }
  | "^" { EXP }
  | "choose" { NCR }
  | "and" { AND }
  | "or" { OR }
  | "<" { LESSTHAN }
  | "/<" { NOTLESSTHAN }
  | "<=" { LESSEQ }
  | "/<=" { NOTLESSEQ }
  | ">" { GREATERTHAN }
  | "/>" { NOTGREATERTHAN }
  | ">=" { GREATEREQ }
  | "/>=" { NOTGREATEREQ }
  | "in" { ELEMENTOF }
  | "sub" { SUBSET }
  | "nsub" { NOTSUBSET }
  | "subeq" { SUBSETEQ }
  | "nsubeq" { NOTSUBSETEQ }
  | "=" { EQ }
  | "sim" { SIM }
  | "!=" { NEQ }
  | "nsim" { NSIM }
  | "approx" { APPROX }
  | "sum" { SUM }
  | "integ" { INTEGRAL }
  | "deriv" { DERIV }
  | "parderiv" { PARDERIV }
  | "metadata" { METADATA }
  | "author" { AUTHOR }
  | "date" { DATE }
  | "title" { TITLE }
  | "settings" { SETTINGS }
  | "pagestyle" { PAGESTYLE }
  | "letter" { LETTER }
  | "A4" { A4 }
  | "legal" { LEGAL }
  | "pageorient" { PAGEORIENT }
  | "portrait" { PORTRAIT }
  | "landscape" { LANDSCAPE }
  | "fontstyle" { FONTSTYLE }
  | "times" { TIMES } 
  | "default" { DEFAULT }
  | "fontsize" { FONTSIZE }
  | "linespacing" { SPACING }
  | "single" { SINGLE }
  | "onepointfive" { ONEPOINTFIVE }
  | "double" { DOUBLE }
  | "marginsize" { MARGINSIZE }
  | "text" { TEXT }
  | "stlc" { STLC }
  | "sysf" { SYSF }
  | "infer" { INFERENCE }
  | "lambda" { LAMBDA }
  | "math" { MATH }
  | "equation" { EQUATION }
  | "table" { TABLE }
  | ";" { SEMI }
  | ":" { COLON }
  | eof { EOF }
  | _ as c {
            printf "Error at line %d\n" !lineno;
            printf "Unrecognized character: [%c]\n" c;
            exit 1 }