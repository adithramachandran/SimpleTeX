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
%token SETTINGS
%token TEXT
%token EQUATION
%token TABLE
%token METADATA
%token INFERENCE
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
%token LAMBDA
%token SMALL
%token BIG
%token MULTI
%token NOTSMALL
%token NOTBIG
%token NOTMULTI

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
    ;

infer_list:
    | i = infer; COMMA; il = infer_list { i :: il }
    | i = infer { [i] }
    ;

infer:
    | LCURLY; m = mapping; RCURLY; LCURLY; t = CONTENT; RCURLY { Axiom (m,t) }
    | LCURLY; il = infer_list; RCURLY; LCURLY; m = mapping; RCURLY; LCURLY; t = CONTENT; RCURLY { Rule (il, m, t) }
    ;

mapping:
    | d1 = delimiter; b1 = block; COMMA; b2 = block; d2 = delimiter; mt = maptype; d3 = delimiter; b3 = block; COMMA; b4 = block; d4 = delimiter { StoreMapping (d1,b1,b2,d2,mt,d3,b3,b4,d4) }
    | p = CONTENT; m = mapping; q = CONTENT { Hoare (p,m,q) }
    ;

delimiter:
    | LANGLE { Langle }
    | RANGLE { Rangle }
    ;

block:
    | s = specialchar { SpecialChar (s) }
    | c = CONTENT { Str (c) }
    ;

specialchar:
    | SIGMA { Sigma }
    | LAMBDA { Lambda }
    ;

maptype:
    | SMALL { SmallStep }
    | BIG { BigStep }
    | MULTI { MultiStep }
    | NOTSMALL { NotSmallStep }
    | NOTBIG { NotBigStep }
    | NOTMULTI { NotMultiStep }
    ;

table:
    | { Row ([]) }
    ;
