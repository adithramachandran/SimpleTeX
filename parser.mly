%{
open Ast
open Printf
open Lexing

let merge (fn,pos1,_) (_,_,pos2) = (fn,pos1,pos2)
%}

%token EOF
%token <string> CONTENT
%token SEMI
%token COLON
%token LCURLY
%token RCURLY
%token COMMA
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token SETTINGS
%token TEXT
%token EQUATION
%token TABLE
%token METADATA
%token MATH
%token INFERENCE
%token LAMBDA
%token PAGESTYLE
%token PAGEORIENT
%token MARGINSIZE
%token FONTSIZE
%token FONTSTYLE
%token SPACING
%token LETTER
%token A4
%token LEGAL
%token PORTRAIT
%token LANDSCAPE
%token TIMES
%token DEFAULT
%token SINGLE
%token ONEPOINTFIVE
%token DOUBLE
%token AUTHOR
%token DATE
%token TITLE
%token LANGLE
%token RANGLE
%token SIGMA
%token SIGMAPRIME
%token SIGMADOUBLEPRIME
%token LAM
%token TAU
%token TAUPRIME
%token TAUZERO
%token TAUONE
%token TAUTWO
%token SMALL
%token BIG
%token MULTI
%token NOTSMALL
%token NOTBIG
%token NOTMULTI
%token PLUS
%token MULT
%token PERIOD

%start <Ast.environment> prog

%%

prog:
    | e = expr; EOF { e }
    | e1 = expr; SEMI; e2 = prog; EOF { ListEnv(e1, e2) }
    ;

expr:
    | LBRACKET; METADATA; RBRACKET; LCURLY; md = metadata_list; RCURLY { Metadata (md) }
    | LBRACKET; SETTINGS; RBRACKET; LCURLY; settings = setting_list; RCURLY { Settings (settings) }  
    | LBRACKET; TEXT; RBRACKET; LCURLY; text = text; RCURLY { Text(text) }
    | LBRACKET; EQUATION; RBRACKET; LCURLY; equation = equation_list; RCURLY { EquationEnv (equation) }
    | LBRACKET; TABLE; RBRACKET; LCURLY; table = table; RCURLY { Table (table) }
    ;

metadata_list:
    | m = meta { m }
    | m1 = meta; SEMI; m2 = metadata_list { MetadataList (m1, m2) }
    ;

meta:
    | AUTHOR; COLON; aname = CONTENT { Author (aname) }
    | DATE; COLON; date = CONTENT { Date (date) }
    | TITLE; COLON; title = CONTENT { Title (title) }
    ;

setting_list:
    | s = setting { s }
    | s1 = setting; SEMI; s2 = setting_list { ListSetting (s1, s2) }
    ;

setting:
    | PAGESTYLE; COLON; pstyle = pagestyle { PageStyle (pstyle) }
    | PAGEORIENT; COLON; porient = pageorient { PageOrient (porient) }    
    | MARGINSIZE; COLON; msize = CONTENT { MarginSize (msize) }
    | FONTSIZE; COLON; fsize = CONTENT { FontSize (fsize) }
    | FONTSTYLE; COLON; fstyle = fontstyle { FontStyle (fstyle) }
    | SPACING; COLON; spacing = spacing { Spacing (spacing) }
    ;

pagestyle:
    | LETTER {Letter}
    | A4 {A4}
    | LEGAL {Legal}
    ;

pageorient:
    | PORTRAIT {Portrait}
    | LANDSCAPE {Landscape}
    ;

fontstyle:
    | TIMES {Times}
    | DEFAULT {Default}
    ;

spacing:
    | SINGLE {Single}
    | ONEPOINTFIVE {OnePointFive}
    | DOUBLE {Double}
    ;

text:
    | content = CONTENT { NormalText (content) }
    | t1 = CONTENT; SEMI; t2 = text { TextList (t1, t2) }
    ;

equation_list:
    | e = equation { Equation (e) }
    | e1 = equation; SEMI; e2 = equation_list { EquationList (e1, e2) }
    ;

equation:
    | INFERENCE; COLON; i = infer { Infer (i) }
    | LAMBDA; COLON; LCURLY; l = lambda; RCURLY { Lambda (l) }
    | MATH; COLON; LCURLY; m = split; RCURLY { MathEquation (m) }
    ;

split:
    | m = math_eq { MathEq (m) }
    ;

math_eq:
    | { Int ("5") }
    ;

infer_list:
    | i = infer; COMMA; il = infer_list { i :: il }
    | i = infer { [i] }
    ;

infer:
    | LCURLY; c = CONTENT; RCURLY { Str (c) }
    | LCURLY; m = mapping; RCURLY { Mapping (m) }
    | LCURLY; m = mapping; RCURLY; LCURLY; t = CONTENT; RCURLY { Axiom (m,t) }
    | LCURLY; l = lambda; RCURLY { LambdaRule (l) }
    | LCURLY; il = infer_list; RCURLY; LCURLY; m = mapping; RCURLY; LCURLY; t = CONTENT; RCURLY { Rule (il, m, t) }
    ;

mapping:
    | mh1 = mapping_half; mt = maptype; mh2 = mapping_half { StoreMapping (mh1,mt,mh2) }
    | p = CONTENT; m = mapping; q = CONTENT { Hoare (p,m,q) }
    ;

mapping_half:
    | d1 = delimiter; b1 = block; COMMA; b2 = block; d2 = delimiter { Pair (d1,b1,b2,d2) }
    | p = CONTENT { MapStr (p) }
    ;

delimiter:
    | LANGLE { Langle }
    | RANGLE { Rangle }
    ;

block:
    | s = specialchar { SpecialChar (s) }
    | c = CONTENT { BlockStr (c) }
    | c = CONTENT; COLON; t = var_type { TypedBlock (c,t) }
    ;

specialchar:
    | SIGMA { Sigma }
    | SIGMAPRIME { SigmaPrime }
    | SIGMADOUBLEPRIME { SigmaDoublePrime }
    | LAM { Lambda }
    ;

maptype:
    | SMALL { SmallStep }
    | BIG { BigStep }
    | MULTI { MultiStep }
    | NOTSMALL { NotSmallStep }
    | NOTBIG { NotBigStep }
    | NOTMULTI { NotMultiStep }
    ;

var_type:
    | c = CONTENT { StrType (c) }
    | TAU { Tau }
    | TAUPRIME { TauPrime }
    | TAUZERO { TauZero }
    | TAUONE { TauOne }
    | TAUTWO { TauTwo }
    | LPAREN; vt1 = var_type; RPAREN; PLUS; LPAREN; vt2 = var_type; RPAREN { FuncType (vt1,Sum,vt2) }
    | LPAREN; vt1 = var_type; RPAREN; MULT; LPAREN; vt2 = var_type; RPAREN { FuncType (vt1,Product,vt2) }
    | LPAREN; vt1 = var_type; RPAREN; SMALL; LPAREN; vt2 = var_type; RPAREN { FuncType (vt1,Func,vt2) }
    ;

lambda:
    | l = lambda_args; PERIOD; b = block { LambdaType (l,b) }
    ;

lambda_args:
    | sc = specialchar; b = block { LambdaArgs (sc,b,None) }
    | sc = specialchar; b = block; la = lambda_args { LambdaArgs (sc,b,Some(la)) }
    ;

table:
    | { Row ([]) }
    ;
