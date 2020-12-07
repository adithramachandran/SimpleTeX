(* 
    This file describes the AST for a SimpleTeX file
*)

(* Parsing information: ((l1,c1),(l2,c2)) represents a symbol
   appearing at line l1 character c1 to line l2 character c2. *)
type info = (int * int) * (int * int)

exception SyntaxError

(** Types [param] and [content] are used for text parameters and text *)
type param = string
type content = string

(** Type [metadata] represents the various kinds of metadata in a LaTeX file *)
type metadata =
  | Author of content
  | Date of content
  | Title of content
  | MetadataList of metadata * metadata

(** Type [pagestyle] represents the different page styles available *)
type pagestyle = 
  | Letter
  | A4
  | Legal

(** Type [pageorient] represents different page orientations *)
type pageorient =
  | Portrait
  | Landscape

(** Type [fontstyle] represents different font styles *)
type fontstyle =
  | Times
  | Default

(** Type [linespacing] represents different amounts of line spacing *)
type linespacing =
  | Single
  | OnePointFive
  | Double

(** Type [setting] represents different kinds of settings that a user can change *)
type setting =
  | PageStyle of pagestyle
  | PageOrient of pageorient
  | MarginSize of param
  | FontSize of param
  | FontStyle of fontstyle
  | Spacing of linespacing
  | ListSetting of setting * setting
 
(** Type [text] represents text environments, either a text environment or multiple lines of text *)
type text =
  | NormalText of content
  | TextList of content * text

(** Type [pageorient] represents the different kinds of inference rules *)
type infer =
  | Str of content
  | Mapping of mapping
  | Axiom of mapping * content
  | Rule of infer list * mapping * content

(** Type [mapping] represents the different kinds of mappings *)
and mapping =
  | StoreMapping of mapping_half * maptype * mapping_half
  | Hoare of content * mapping * content

(** Type [mapping_half] represents either pairs or strings,
    and two mapping_halves make a mapping *)
and mapping_half=
  | Pair of delimiter * block * block * delimiter
  | MapStr of content

(** Type [delimiter] represents angle brackets used in inference rules *)
and delimiter =
  | Langle
  | Rangle

(** Type [maptype] represents the different kinds of relations,
    including small step, big step, and multistep *)
and maptype =
  | SmallStep
  | BigStep
  | MultiStep
  | NotSmallStep
  | NotBigStep
  | NotMultiStep

(** Type [specialchar] represents different special characters *)
and specialchar =
  | Sigma
  | Lambda
  | SigmaPrime
  | SigmaDoublePrime

(** Type [block] represents either a spacial character or a string.
    Two blocks make up a pair, enclosed with delimiters *)
and block =
  | SpecialChar of specialchar
  | BlockStr of content

(** Type [math_eq] represents the different kinds of mathematical equations
    available. *)
type math_eq = 
  | Int of content
  | Var of content
  | Fact of math_eq * unop
  | Not of unop * math_eq
  | Binop of math_eq * binop * math_eq
  | Relation of math_eq * relation * math_eq
  | Sum of math_eq * math_eq * math_eq
  | Integ of math_eq * math_eq * math_eq * math_eq
  | Deriv of math_eq * math_eq

(** Type [unop] represents the different kinds of unary operations *)
and unop =
  | Fact
  | Not

(** Type [binop] represents the different kinds of binary operations *)
and binop = 
  | Plus
  | Minus
  | Mult
  | Div
  | Exp
  | NCR
  | And
  | Or

(** Type [relation] represents the different kinds of logical and set relations
    between objects *)
and relation =
  | Lt
  | Nlt
  | Leq
  | Nleq
  | Gt
  | Ngt
  | Geq
  | Ngeq
  | Subset
  | NSubset
  | SubsetEq
  | NSubsetEq
  | Eq
  | Sim
  | Neq
  | Nsim
  | Approx

(** Type [simpleequation] represents a simple equation, which is a building block of a list of equations *)
type simpleequation = 
  | Infer of infer
  | MathEquation of math_eq

(** Type [equation] represents either a simple equation or a simple equation prepended onto a list of equations *)
type equation =
  | Equation of simpleequation
  | EquationList of simpleequation * equation

(** Type [table] represents a table *)
type table =
  | Row of text list
  | Table of table * table

(** Type [environment] represents the various kinds of environements in the SimpleTeX language *)
type environment =
  | Metadata of metadata
  | Settings of setting
  | Text of text
  | EquationEnv of equation
  | Table of table
  | ListEnv of environment * environment
  | Nil