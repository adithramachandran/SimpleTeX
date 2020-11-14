(* CS 4110 Homework 3
   This file defines the abstract syntax tree (AST) for the IMP language. You
   shouldn't need to modify it. *)

(* Parsing information: ((l1,c1),(l2,c2)) represents a symbol
   appearing at line l1 character c1 to line l2 character c2. *)
type info = (int * int) * (int * int)

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
  | ListSetting of setting * setting
 
type text =
  | NormalText of content
  (* | Bold of content
  | Italics of content *)

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
  | Deriv of equation * aexp
  | Integ of equation * equation * equation * aexp
  | Summation of equation * equation * equation
  | Quality of bop * equation * equation

type table =
  | Row of text list
  | Table of table * table

type environment =
  | Settings of setting
  | Text of text
  | Equation of equation
  | Table of table
  | ListEnv of environment * environment
  | Nil