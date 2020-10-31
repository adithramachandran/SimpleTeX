(**
  This is the Abstract Search Tree that represents the
  SimpleTeX language
*)

exception SyntaxError

type param = string
type content = string

type setting =
  | PageStyle of param
  | PageOrient of param
  | MarginSize of param
  | FontSize of param
  | FontStyle of param
  | Spacing of param
 
type text =
  | Normal of content
  | Bold of content
  | Italics of content

type bop =
  | Plus
  | Minus
  | Times
  | Divide
  | Less
  | Greater
  | And
  | Or

type uop =
  | Fact
  | Not

type aexp =
  | Int of int
  | Binop of bop * aexp * aexp
  | Unop of uop * aexp * aexp

type equation =
  | Arith of aexp
  | Inference of equation * equation
  | Deriv of aexp * text
  | Integ of aexp * aexp * text
  | Summation of text * aexp * aexp * aexp
  | Equality of equation * equation
  | Inequality of bop * equation * equation

type table =
  | Row of text list
  | Table of table * table

type environment =
  | Settings of setting list
  | Text of text
  | Equation of equation
  | Table of table