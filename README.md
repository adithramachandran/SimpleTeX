> TODO: Replace all underscores

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

## <a name="set"></a> settings
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

## <a name="met"></a> metadata
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

## <a name="tex"></a> text
The **text** environment contains a list of **quote delimited** strings, separated by **semicolons**. These strings may also contain LaTeX code.

```
[text] {
    "This is some SimpleTeX text!";
    "This is some \emph{emphasized SimpleTeX text}!";
    "This is some SimpleTeX text with $\tau_1\rightarrow\tau_2$ an in-line equation"
}...
```

## <a name="equ"></a> equation
<span style="color:#eeeeee">All special characters are preceded by an <b>underscore</b>.</span>

In all documentation below, a **quote delimited** string will be denoted as **S**.

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
* **S**:**S**
* **S**:**SC**
* **S**:**S**.**S or SC**
* **S**:(**S or SC**)+(**S or SC**)
* **S**:(**S or SC**)\*(**S or SC**)
* **S**:(**S or SC**)->(**S or SC**)

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
* -*>