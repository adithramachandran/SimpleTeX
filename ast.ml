(**
  This is the Abstract Search Tree that represents the
  SimpleTeX language
*)

exception SyntaxError

type param = string
type content = string

type pagestyle = 
  | Letter
  | A4

type pageorient =
  | Portrait
  | Landscape

type fontstyle =
  | Times
  | Arial
  | Cambria

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
 
type text =
  | Normal of content
  | Bold of content
  | Italics of content

type bop =
  | Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Less
  | Greater
  | Leq
  | Geq
  | And
  | Or

type unop =
  | Fact
  | Not

type aexp =
  | Int of int
  | Bool of bool
  | Var of content
  | Binop of bop * aexp * aexp
  | Unop of unop * aexp
  | Frac of aexp * aexp

type equation =
  | Arith of aexp
  | Separator of bop
  | Mapping of content * content
  | Inference of (equation list) * equation
  | Deriv of aexp * aexp
  | Integ of aexp * aexp * aexp * aexp
  | Summation of aexp * aexp * aexp * aexp
  | Quality of bop * equation * equation

type table =
  | Row of text list
  | Table of table * table

type environment =
  | Settings of setting list
  | Text of text
  | Equation of equation list
  | Table of table

type document =
  | Nil
  | Document of environment * document