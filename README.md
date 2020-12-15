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

Note that the terminal environment does not have a **semicolon** after it! Click on a link below to see the syntax for the corresponding environment, and note that if a value is required to be **quote delimited**, it will be depicted as such:

### [settings](#settings)
### [metadata](#metadata)
### [text](#text)
### [equation](#equations)
### [table](#tables)

## <a name="settings"></a> settings
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

## <a name="metadata"></a> metadata
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

## <a name="text"></a> text
The **text** environment contains a list of **quote delimited** strings, separated by **semicolons**. These strings may also contain LaTeX code.

```
[text] {
    "This is some SimpleTeX text!";
    "This is some \emph{emphasized SimpleTeX text}!";
    "This is some SimpleTeX text with $\tau_1\rightarrow\tau_2$ an in-line equation"
}...
```

## <a name="equations"></a> equation
Click the links below to look at constructs that are used across different equation subtypes:

#### [context](#context)