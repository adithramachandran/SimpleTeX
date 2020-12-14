%{
open Ast
open Printf
open Lexing

let merge (fn,pos1,_) (_,_,pos2) = (fn,pos1,pos2)
%}

%token EOF
%token <string> CONTENT
%token <string> INT
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
%token INTEGRAL
%token DERIV
%token SUM
%token FRAC
%token INFERENCE
%token STLC
%token SYSF
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
%token BIGLAM
%token TAU
%token TAUPRIME
%token TAUZERO
%token TAUONE
%token TAUTWO
%token GAMMA
%token DELTA
%token FORALL
%token SMALL
%token BIG
%token MULTI
%token NOTSMALL
%token NOTBIG
%token NOTMULTI
%token FACT
%token NOT
%token PLUS
%token MULT
%token MINUS
%token DIV
%token EXP
%token NCR
%token AND
%token OR
%token LESSTHAN
%token NOTLESSTHAN
%token LESSEQ
%token NOTLESSEQ
%token GREATERTHAN
%token NOTGREATERTHAN
%token GREATEREQ
%token NOTGREATEREQ
%token ELEMENTOF
%token SUBSET
%token NOTSUBSET
%token SUBSETEQ
%token NOTSUBSETEQ
%token EQ
%token SIM
%token NEQ
%token NSIM
%token APPROX
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
    | STLC; COLON; s = stlc_rule { STLCRule (s) }
    | SYSF; COLON; s = sysf_rule { SystemFRule (s) }
    | MATH; COLON; LCURLY; m = split; RCURLY { MathEquation (m) }
    ;

split:
    | m = math_eq { MathEq (m) }
    | m = math_eq; SEMI; ml = split { MathEqList (m, ml) }
    ;

math_eq:
    | i = INT { Int (i) }
    | c = CONTENT { Var (c) }
    | u = unop; c = CONTENT { NotEq (u,Var(c)) }
    | m = math_eq; u = unop { FactEq (u,m) }
    | m1 = math_eq; b = binop; m2 = math_eq { Binop (m1,b,m2) }
    | SUM; LCURLY; m1 = math_eq; RCURLY; LCURLY; m2 = math_eq; RCURLY; LCURLY; m3 = math_eq; RCURLY { Sum (m1,m2,m3) }
    | INTEGRAL; LCURLY; m1 = math_eq; RCURLY; LCURLY; m2 = math_eq; RCURLY; LCURLY; m3 = math_eq; RCURLY; LCURLY; m4 = math_eq; RCURLY { Integ (m1,m2,m3, m4) }
    | DERIV; LCURLY; m1 = math_eq; RCURLY; LCURLY; m2 = math_eq; RCURLY { Deriv (m1,m2) }
    | FRAC; LCURLY; m1 = math_eq; RCURLY; LCURLY; m2 = math_eq; RCURLY { Frac (m1,m2) } 
    ;

unop:
    | FACT { Fact }
    | NOT { Not }
    ;

binop:
    | PLUS { Operation (Plus) }
    | MINUS { Operation (Minus) }
    | MULT { Operation (Mult) }
    | DIV { Operation (Div) }
    | EXP { Operation (Exp) }
    | NCR { Operation (NCR) }
    | AND { Operation (And) }
    | OR { Operation (Or) }
    | LESSTHAN { Relation (Lt) }
    | NOTLESSTHAN { Relation (Nlt) }
    | LESSEQ { Relation (Leq) }
    | NOTLESSEQ { Relation (Nleq) }
    | GREATERTHAN { Relation (Gt) }
    | NOTGREATERTHAN { Relation (Ngt) }
    | GREATEREQ { Relation (Geq) }
    | NOTGREATEREQ { Relation (Ngeq) }
    | ELEMENTOF { Relation (ElementOf) }
    | SUBSET { Relation (Subset) }
    | NOTSUBSET { Relation (NSubset) }
    | SUBSETEQ { Relation (SubsetEq) }
    | NOTSUBSETEQ { Relation (NSubsetEq) }
    | EQ { Relation (Eq) }
    | SIM { Relation (Sim) }
    | NEQ { Relation (Neq) }
    | NSIM { Relation (Nsim) }
    | APPROX { Relation (Approx) }
    ;

infer_list:
    | i = infer; COMMA; il = infer_list { i :: il }
    | i = infer { [i] }
    ;

infer:
    | LCURLY; c = CONTENT; RCURLY { Str (c) }
    | LCURLY; m = mapping; RCURLY { Mapping (m) }
    | LCURLY; m = mapping; RCURLY; LCURLY; t = CONTENT; RCURLY { Axiom (m,t) }
    | LCURLY; il = infer_list; RCURLY; LCURLY; m = mapping; RCURLY; LCURLY; t = CONTENT; RCURLY { Rule (il, m, t) }
    ;

stlc_rule:
    | LCURLY; c = context; COMMA; srb = stlc_rule_block; RCURLY { STLCLambda (c,srb) }
    | LCURLY; c = context; COMMA; srb = stlc_rule_block; RCURLY; LCURLY; name = CONTENT; RCURLY { STLCAxiom (c,srb,name) }
    | LCURLY; stlc_l = stlc_premise_list; RCURLY; LCURLY; c = context; COMMA; srb = stlc_rule_block; RCURLY; LCURLY; name = CONTENT; RCURLY { STLCTree (stlc_l,c,srb,name) }
    ;

stlc_premise_list:
    | s = stlc_premise { [s] }
    | s = stlc_premise; COMMA; sl = stlc_premise_list { s :: sl }
    ;

stlc_premise:
    | c = CONTENT { StrSTLCPremise (c) }
    | c = context; b = block { STLCPremise (c,b) }
    ;

stlc_rule_block:
    | b = block { STLCRuleBlock (b) }
    | l = lambda { STLCRuleLambda (l) }
    ;

sysf_rule:
    | LCURLY; tc = type_context; COMMA; c = context; COMMA; srb = sysf_rule_block; RCURLY { SystemFLambda (tc,c,srb) }
    | LCURLY; tc = type_context; COMMA; c = context; COMMA; srb = sysf_rule_block; RCURLY; LCURLY; name = CONTENT; RCURLY { SystemFAxiom (tc,c,srb,name) }
    | LCURLY; sysf_l = sysf_premise_list; RCURLY; LCURLY; tc = type_context; COMMA; c = context; COMMA; srb = sysf_rule_block; RCURLY; LCURLY; name = CONTENT; RCURLY { SystemFTree (sysf_l,tc,c,srb,name) }
    ;

sysf_rule_block:
    | b = block { SystemFRuleBlock (b) }
    | l = lambda { SystemFRuleLambda (l) }
    ;

sysf_premise_list:
    | s = sysf_premise { [s] }
    | s = sysf_premise; COMMA; sl = sysf_premise_list { s :: sl }
    ;

sysf_premise:
    | c = CONTENT { StrSystemFPremise (c) }
    | tc = type_context; c = context; b = block { SystemFPremise (tc,c,b) }
    ;

context:
    | LPAREN; RPAREN { EmptyContext }
    | LPAREN; GAMMA; RPAREN { Gamma }
    | LPAREN; GAMMA; COMMA; bl = block_list; RPAREN { GammaList (bl) }
    ;

type_context:
    | LPAREN; RPAREN; { EmptyTypeContext }
    | LPAREN; DELTA; RPAREN; { Delta }
    | LPAREN; DELTA; COMMA; bl = block_list; RPAREN { DeltaUnion (bl) }
    ;

block_list:
    | b = block { [b] }
    | b = block; COMMA; bl = block_list { b :: bl }
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
    | BIGLAM { BigLambda }
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
    | FORALL; c = CONTENT; PERIOD; vt = var_type { Universal (c,vt) }
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
