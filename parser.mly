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
%token LBRACKET
%token RBRACKET
%token SETTINGS
%token TEXT
%token EQUATION
%token TABLE
%token METADATA
%token PAGESTYLE
%token PAGEORIENT
%token MARGINSIZE
%token FONTSIZE
%token FONTSTYLE
%token SPACING
%token LETTER
%token A4
%token PORTRAIT
%token LANDSCAPE
%token TIMES
%token ARIAL
%token CAMBRIA
%token SINGLE
%token ONEPOINTFIVE
%token DOUBLE
%token AUTHOR
%token DATE
%token TITLE

%start <Ast.environment> prog

%%

prog:
    | e = expr; EOF { e }
    | e1 = expr; SEMI; e2 = expr; EOF { ListEnv(e1, e2) }
    ;

expr:
    | LBRACKET; METADATA; RBRACKET; LCURLY; md = metadata_list; RCURLY { Metadata (md) }
    | LBRACKET; SETTINGS; RBRACKET; LCURLY; settings = setting_list; RCURLY { Settings (settings) }  
    | LBRACKET; TEXT; RBRACKET; LCURLY; text = text; RCURLY { Text(text) }
    | LBRACKET; EQUATION; RBRACKET; LCURLY; equation = equation; RCURLY { Equation (equation) }
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
    | PAGESTYLE; COLON; LCURLY; pstyle = pagestyle; RCURLY { PageStyle (pstyle) }
    | PAGEORIENT; COLON; LCURLY; porient = pageorient; RCURLY { PageOrient (porient) }    
    | MARGINSIZE; COLON; LCURLY; msize = CONTENT; RCURLY { MarginSize (msize) }
    | FONTSIZE; COLON; LCURLY; fsize = CONTENT; RCURLY { FontSize (fsize) }
    | FONTSTYLE; COLON; LCURLY; fstyle = fontstyle; RCURLY { FontStyle (fstyle) }
    | SPACING; COLON; LCURLY; spacing = spacing; RCURLY { Spacing (spacing) }
    ;

pagestyle:
    | LETTER {Letter}
    | A4 {A4}
    ;

pageorient:
    | PORTRAIT {Portrait}
    | LANDSCAPE {Landscape}
    ;

fontstyle:
    | TIMES {Times}
    | ARIAL {Arial}
    | CAMBRIA {Cambria}
    ;

spacing:
    | SINGLE {Single}
    | ONEPOINTFIVE {OnePointFive}
    | DOUBLE {Double}
    ;

text:
    | content = CONTENT { NormalText (content) }
    | t1 = text; SEMI; t2 = text { TextList (t1, t2) }
    ;

equation:
    | { Arith(Int(1))}
    ;

table:
    | { Row ([]) }
    ;
