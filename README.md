# SimpleTeX: A Simplified Typesetting Language
SimpleTeX is a typesetting language built for speed and brevity, and outputs LaTeX code!

# Running Your SLTX File
**.sltx** is the extension recognized by the SimpleTeX transpiler. To output a .txt file with the corresponding LaTeX code, simply cd into the SimpleTeX folder and type **make** into terminal. Then, for a file **x.sltx**, type **./sltx x.sltx** into the terminal, and it will output the corresponding LaTeX file into a file called **x.txt**. You can then paste this code into **OverLeaf** and compile.

# Syntax
Each SimpleTeX file is separated into a list of **environments**. Given 3 environments, **e1**, **e2**, and **e3**, the SimpleTeX document will be formatted as below:

```
[e1] {
    ...code for e1...
};
[e2] {
    ...code for e2...
};
[e3] {
    ...code for e3...
}
```

Note that the terminal environment does not have a **semicolon** after it! Click on a link below to see the syntax for the corresponding environment, and note that if a value is required to be **quote delimited**, it will be depicted as such. If anything is unclear after reading the documentation, please view **sample.sltx** for examples of everything discussed below.

### [settings](#set)
### [metadata](#met)
### [text](#tex)
### [equation](#equ)
### [table](#tab)

# <a name="set"></a> settings
The **settings** environment must be the **first** environment in a SimpleTeX document, or must be omitted altogether. For 3 settings **s1**, **s2**, and **s3**, the format for a settings environment is as below:

```
[settings] {
    s1: ...code for s1...;
    s2: ...code for s2...;
    s3: ...code for s3
}...
```

The following settings are available:

### fontsize
**fontsize** must be the first setting in the **settings** environment. The value must be a **quote delimited string** with the following possible values:
* "8pt"
* "9pt"
* "10pt"
* "11pt"
* "12pt"
* "14pt"
* "17pt"
* "20pt"
The remaining settings can be entered in any order.

### pagestyle
* letter
* a4
* legal

### pageorient
* portrait
* landscape

### marginsize
**marginsize** must contain a **quote delimited** string with a numerical value followed by a unit. For example, "1.5in", "4.5em", or "2cm".

### fontstyle
If **fontsize** is not mentioned, the default LaTeX font will be used.
* times
* default

### linespacing
* single
* onepointfive
* double

# <a name="met"></a> metadata
The **metadata** environment must be the **second** environment in a SimpleTeX document, or the **first** if the **settings** environment has been omitted, or must be omitted altogether. For 3 metadata tags **m1**, **m2**, and **m3**, the format for a settings environment is as below:

```
[metadata] {
    m1: ...code for m1...;
    m2: ...code for m2...;
    m3: ...code for m3
}...
```

The following metadata tags are available:

### author
A **quote delimited** string containing the author information, "John Doe" for example.

### date
A **quote delimited** string containing the date, "January 1st, 2000" for example.

### title
A **quote delimited** string containing the title, "My First SimpleTeX Document" for example.

# <a name="tex"></a> text
The **text** environment contains a list of **quote delimited** strings, separated by **semicolons**. These strings may also contain LaTeX code.

```
[text] {
    "This is some SimpleTeX text!";
    "This is some \emph{emphasized SimpleTeX text}!";
    "This is some SimpleTeX text with $\tau_1\rightarrow\tau_2$ an in-line equation"
}...
```

# <a name="equ"></a> equation
All special characters are preceded by an <b>underscore</b>.

In all documentation below, a **quote delimited** string will be denoted as **S**, and a number, which includes positive and negative integers and floats, will be denoted as **N**.

For 3 equations **e1**, **e2**, and **e3**, the format for an equation environment is as below:

```
[equation] {
    e1: ...code for e1...;
    e2: ...code for e2...;
    e3: ...code for e3
}...
```

Click the links below to look at constructs that are used across different equation subtypes:

#### [blocks](#blo)
#### [special characters](#sc)
#### [contexts](#con)
#### [type_contexts](#tcon)
#### [delimiters](#del)
#### [maptypes](#mapt)

### <a name="sc"></a>special characters
Special characters are used in almost every type of equation. They are:
* _sig - sigma
* _sig' - sigma'
* _sig'' - sigma''
* _lam - lambda
* _Lam - capital lambda
* _Gamma - capital gamma
* _Delta - capital delta
* _tau - tau
* _tau' - tau'
* _tau0 - tau_0
* _tau1 - tau_1
* _tau2 - tau_2

In all subsequent documentation, special characters will be referred to as **SC**.

### <a name="blo"></a>blocks
Blocks are the building blocks to many other equation types. They can be:
* **SC**
* **S**
* **S** : **S**
* **S** : **SC**
* **S** : **S** . **S or SC**
* **S** : (**S or SC**) + (**S or SC**)
* **S** : (**S or SC**) * (**S or SC**)
* **S** : (**S or SC**) -> (**S or SC**)

All blocks with colons indicated typed expressions. For instance, "x":(("int")->("int"))->("int") indicates that x has type (int->int)->int. In all subsequent documentation, blocks will be referred to as **B**.

### <a name="con"></a>contexts
Contexts can be:
* ()
* (_Gamma)
* (_Gamma, **B1**, **B2**, ........)

In all subsequent documentation, contexts will be referred to as **C**.

### <a name="tcon"></a>type_contexts
Type Contexts can be:
* ()
* (_Delta)
* (_Delta, **B1**, **B2**, ........)

In all subsequent documentation, type contexts will be referred to as **TC**.

### <a name="del"></a> delimiters
Delimiters include:
* << - left angle bracket
* \>\> - right angle bracket

In all subsequent documentation, delimiters will be referred to as **D**.

### <a name="mapt"></a>maptypes
Map Types include:
* -> - small step
* => - big step
* ->* - multi step
* -/> - does not small step
* =/> - does not big step
* -/>* - does not multi step

In all subsequent documentation, map types will be referred to as **MT**.

## Equations:
The links below will take you to the corresponding equation type:
### [inference rules](#inf)
### [STLC rules](#stlc)
### [System-F rules](#sysf)
### [Lambda equations](#lambda)
### [mathematical equations](#math)

## <a name="inf"> infer
The **infer** equation can contain the following:
* {**S**}
* {**D** **B** **B** **D** **MT** **D** **B** **B** **D**} - standard mapping
* {**S1**    **D** **B** **B** **D** **MT** **D** **B** **B** **D**    **S2**} - Hoare triple with precondition **S1** and postcondition **S2**
* {standard mapping or Hoare triple}{**S**} - an axiom titled **S**
* {one or more standard mappings, hoare triples, or inferences, each separated by a comma}{a standard mapping or Hoare triple}{**S**} - A proof tree with the premises in the first set of curly braces, the conclusion in the second set of curly braces, with title **S**

## <a name="lambda"></a> lambda
The **lambda** equation is defined as {one or more (_lam or _Lam **B**) pairs **.** **B**}, which is a lambda with arguments described by the (_lam or _Lam **B**) pairs and body **B**.

All lambdas will be referenced using **L** henceforth.

## <a name="stlc"> stlc
The **stlc** equation is defined as:
* {**C ,** (**L** or **B**)} - a standard STLC lambda
* {a standard STLC lambda}{**S**} - an STLC Axiom with title **S**
* {a list of one or more comma separated (**S** or (**C** **B**))}{a standard STLC lambda}{**S1**} - an STLC rule with the premises in the first set of curly braces, the conclusion in the second set of curly braces, with title **S1**.

## <a name="sysf"> sysf
The **sysf** equation is defined as:
* {**TC ,** **C ,** (**L** or **B**)} - a standard System-F lambda
* {a standard STLC lambda}{**S**} - a System-F Axiom with title **S**
* {a list of one or more comma separated (**S** or (**TC** **C** **B**))}{a standard System-F lambda}{**S1**} - a System-F rule with the premises in the first set of curly braces, the conclusion in the second set of curly braces, with title **S1**.

## <a name="math"> math
The **math** equation is defined as a list of **semicolon** separated sub-equations. For the sake of brevity, the abbreviation **SE** is used for sub-equations. These sub-equations can be:
* **N**
* **S** -  a variable
* not **S** -  the logical negation of a variable
* **SE**! - the factorial of **SE**
* **SE1** binop **SE2** - a binary operation between **SE1** and **SE2**. binary operations include:
    * +
    * -
    * *
    * /
    * ^
    * ncr - **SE1** choose **SE2**
    * and
    * or
    * <
    * /<
    * <=
    * /<=
    * \>
    * /\>
    * \>=
    * /\>=
    * subset
    * nsubset
    * subseteq
    * nsubseteq
    * =
    * sim
    * !=
    * nsim
    * approx
* sum : {**SE1**}{**SE2**}{**SE3**} - summation notation with lower bound **SE1**, upper bound **SE2**, and body **SE3**
* sum : {**SE1**}{**SE2**} - summation notation with lower bound **SE1** and body **SE2**. Useful for summing over elements of a set.
* integ : {**SE1**}{**SE2**}{**SE3**}{**SE4**} - definite integral with lower bound **SE1**, upper bound **SE2**, body **SE3**, taken with respect to **SE4**
* deriv : {**SE1**}{**SE2**} - derivative of **SE1** with respect to **SE2**
* parderiv : {**SE1**}{**SE2**} - partial derivative of **SE1** with respect to **SE2**
* frac : {**SE1**}{**SE2**} - fraction representation of **SE1**/**SE2**.

# <a name="tab"></a> table
Each cell in a table is either **N** or **S**. In table, let us call a cell **CE**. A table is written as:

```
[table] {
    CE11,CE12,CE13,CE14,......,CE1n;
    ...............................;
    ...............................;
    CEm1,CEm2,CEm3,CEm4,......,CEmn;
}
```
This is a table with **m** rows and **n** columns.