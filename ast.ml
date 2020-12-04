(* CS 4110 Homework 3
   This file defines the abstract syntax tree (AST) for the IMP language. You
   shouldn't need to modify it. *)

(* Parsing information: ((l1,c1),(l2,c2)) represents a symbol
   appearing at line l1 character c1 to line l2 character c2. *)
type info = (int * int) * (int * int)

exception SyntaxError

type param = string
type content = string
type number = string
type boolean = string

type metadata =
  | Author of content
  | Date of content
  | Title of content
  | MetadataList of metadata * metadata

type pagestyle = 
  | Letter
  | A4
  | Legal

type pageorient =
  | Portrait
  | Landscape

type fontstyle =
  | Times
  | Default

type linespacing =
  | Single
  | OnePointFive
  | Double

type setting =
  | PageStyle of pagestyle
  | PageOrient of pageorient
  | MarginSize of param
  | FontSize of param
  | FontStyle of fontstyle
  | Spacing of linespacing
  | ListSetting of setting * setting
 
type text =
  | NormalText of content
  | TextList of content * text

(* type bop =
  | Plus
  | Minus
  | Times
  | Div
  | Eq
  | Lt
  | Gt
  | Leq
  | Geq
  | And
  | Or

type unop =
  | Fact
  | Not *)

type infer =
  | Axiom of mapping * content
  | Rule of infer list * mapping * content

and mapping =
  | StoreMapping of delimiter * block * block * delimiter * maptype * delimiter * block * block * delimiter
  | Hoare of content * mapping * content

and delimiter =
  | Langle
  | Rangle

and maptype =
  | SmallStep
  | BigStep
  | MultiStep
  | NotSmallStep
  | NotBigStep
  | NotMultiStep

and specialchar =
  | Sigma
  | Lambda

and block =
  | SpecialChar of specialchar
  | Str of content

(* type aexp =
  | Int of number
  | Bool of boolean
  | Var of content
  | Binop of bop * aexp * aexp
  | Unop of unop * aexp
  | Frac of aexp * aexp

type simpleequation =
  | Arith of aexp
  | Mapping of content * content
  | Deriv of simpleequation * content
  | Integ of number * number * simpleequation * content
  | Sum of bound * bound option * simpleequation
  | Quality of bop * simpleequation * simpleequation *)

type simpleequation = 
  | Infer of infer

type equation =
  | Equation of simpleequation
  | EquationList of simpleequation * equation

type table =
  | Row of text list
  | Table of table * table

type environment =
  | Metadata of metadata
  | Settings of setting
  | Text of text
  | EquationEnv of equation
  | Table of table
  | ListEnv of environment * environment
  | Nil