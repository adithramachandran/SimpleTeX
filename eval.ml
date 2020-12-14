open Ast
open Printf

(** TODO: Change string delimiter *)

exception MetadataException of string
exception SettingException of string

(** [start_eval file ast] starts the evaluation of the AST [ast] parsed from file [file]
    and outputs a .txt file named [file].txt with the corresponding LaTeX code. *)
let rec start_eval (file:string) (ast:Ast.environment) : unit =
  let (post_settings_ast, header) = make_settings ast [] in
  let (post_metadata_ast, header_with_title) = begin match post_settings_ast with
                               | None -> (None, header)
                               | Some(a) -> (make_title a header) end in
  let base_out = header_with_title @ ["\\begin{document}"; "\\maketitle"; "\\setlength{\\parskip}{1em}"] in
  let out = begin match post_metadata_ast with
                  | None -> base_out
                  | Some(a) -> eval a base_out end in
  let final_out = out @ ("\\end{document}" :: []) in
  let file_name = String.concat "" [(String.sub file 0 (String.index file '.')); ".txt"] in
  let oc = open_out file_name in
  write oc final_out;
  close_out oc;
  print_endline "done!"

(** [write oc list] writes the string list [list] to the out_channel [oc] *)
and write (oc:out_channel) (list:string list) : unit =
  match list with
  | [] -> ()
  | h :: t -> fprintf oc "%s\n" h; write oc t;

(** [make_title ast header] makes the title from [ast] and appends it to [header] *)
and make_title (ast:Ast.environment) (header:string list) : Ast.environment option * string list =
  (** [extract_metadata m l] extracts the metadata represented by Ast.metadata [m]
      and outputs the corresponding string list *)
  let rec extract_metadata (m:Ast.metadata) (l:string list) : string list =
    match m with
    | MetadataList(m1, m2) -> (extract_metadata m1 []) @ (extract_metadata m2 [])
    | Author(name) -> [String.concat "" ["\\author{"; name; "}"]]
    | Date(date) -> [String.concat "" ["\\date{"; date; "}"]]
    | Title(title) -> [String.concat "" ["\\title{"; title; "}"]] in

  match ast with
  | Metadata (m) -> (None, header @ (extract_metadata m []))
  | ListEnv(Metadata(m), a) -> (Some(a), header @ (extract_metadata m []))
  | _ -> (Some(ast), header @ ["\\author{}"; "\\date{}"; "\\title{}"])

(** [make_settings ast header] makes the settings in [ast] and appends them to [header] *)
and make_settings (ast:Ast.environment) (header:string list) : Ast.environment option * string list =
  (** [extract_settings s l] extracts the settings represented by the Ast.setting [s]
      and outputs the corresponding string list *)
  let rec extract_settings (s:Ast.setting) (l:string list) : string list = 
    match s with
    | FontSize(f) -> raise (SettingException "You must either declare fontsize first or omit it")
    | PageStyle(s) ->
      let style = begin match s with
                        | Letter -> "letterpaper"
                        | A4 -> "a4paper"
                        | Legal -> "legalpaper" end in
      [String.concat "" ["\\geometry{"; style; "}"]]
    | PageOrient(o) -> 
      let orient = begin match o with
                        | Portrait -> "portrait"
                        | Landscape -> "landscape" end in
      [String.concat "" ["\\geometry{"; orient; "}"]]
    | MarginSize(m) -> [String.concat "" ["\\geometry{margin="; m; "}"]]
    | FontStyle(s) ->
      begin match s with
          | Default -> [""]
          | Times -> ["\\usepackage{mathptmx}"] end
    | Spacing(l) ->
      begin match l with
          | Single -> [""]
          | OnePointFive -> ["\\onehalfspacing"]
          | Double -> ["\\doublespacing"] end
    | ListSetting(s1, s2) -> (extract_settings s1 []) @ (extract_settings s2 []) in

  (** [extract_fontsize s l] extracts the fontsize from the Ast.setting [s] and outputs the corresponding
      string list, in order to force fontsize being the first defined setting. *)
  let rec extract_fontsize (s:Ast.setting) (l:string list) : string list =
    match s with
    | FontSize(s) -> [String.concat "" ["\\documentclass["; s;"]{extarticle}"]; "\\usepackage{geometry, mathpartir, setspace, amssymb, amsmath, bm}"]
    | ListSetting(FontSize(f), s) -> (extract_fontsize (FontSize(f)) []) @ (extract_settings s [])
    | _ -> ["\\documentclass[12pt]{article}"; "\\usepackage{geometry, mathpartir, setspace, amssymb, amsmath, bm}"] @ (extract_settings s []) in

  match ast with
  | Settings (s) -> (None, extract_fontsize s [])
  | ListEnv(Settings (s), a) -> (Some(a), extract_fontsize s [])
  | _ -> (Some(ast), ["\\documentclass[12pt]{article}"; "\\usepackage{geometry, mathpartir, setspace, amssymb, amsmath, bm}"])

(** [eval_text t] evaluates the Ast.text environment [t] and outputs the corresponding string list *)
and eval_text (t:Ast.text) : string list =
  match t with
  | NormalText(c) -> [c]
  | TextList(t1, t2) -> (eval_text (NormalText(t1))) @ ["\\par"] @ (eval_text t2)

(** [eval_equation e out] evaluates the Ast.equation [e] and outputs the corresponding string list *)
and eval_equation (e:Ast.equation) (out:string list) : string list =
  
  (** [eval_simple_equation se out] evaluates Ast.simpleequation [se] to a string list
      appends it to [out] and returns the result of this append*)
  let rec eval_simple_equation (se:Ast.simpleequation) (out:string list) : string list =
  
    (** [eval_special_character sc] evaluates [sc] to its corersponding string in LaTeX *)
    let eval_special_character (sc:Ast.specialchar) =
      match sc with
      | Sigma -> "\\sigma"
      | Lambda -> "\\lambda"
      | BigLambda -> "\\Lambda"
      | SigmaPrime -> "\\sigma'"
      | SigmaDoublePrime -> "\\sigma''" in

    (** [eval_delim d] evaluates Ast.delimiter [d] to a string *)
    let eval_delim (d:Ast.delimiter) : string =
      match d with
      | Langle -> "\\langle"
      | Rangle -> "\\rangle"
      | Sum -> "+"
      | Product -> "\\times"
      | Func -> "\\rightarrow" in

    (** [eval_block b] evaluates Ast.block [b] to a string *)
    let eval_block (b:Ast.block) : string =

      (** [eval_var_type vt] evaluates Ast.var_type [vt] to the corresponding string in LaTeX *)
      let rec eval_var_type (vt:Ast.var_type) : string = 
        match vt with
        | StrType (c) -> String.concat "" ["\\textbf{"; c; "}"]
        | Tau -> "\\tau"
        | TauPrime -> "\\tau'"
        | TauZero -> "\\tau_0"
        | TauOne -> "\\tau_1"
        | TauTwo -> "\\tau_2"
        | Universal (c, vt) -> String.concat "" ["\\forall "; c; "."; (eval_var_type vt)]
        | FuncType (vt1, d, vt2) -> String.concat "" ["("; eval_var_type vt1; eval_delim d; eval_var_type vt2; ")"] in

      match b with
      | SpecialChar (s) -> eval_special_character s
      | BlockStr (c) -> c
      | TypedBlock (c, vt) ->
        let var_t = eval_var_type vt in
        String.concat "" [c; ":"; var_t] in
  
    (** [eval_lambda l] evaluates the Ast.lambda [l] to its corresponding string in LaTeX *)
    let eval_lambda (l:Ast.lambda) : string =

        let rec eval_lambda_args (la:Ast.lambda_args) =
          match la with
          | LambdaArgs (sc, b, lao) ->
            let spec = eval_special_character sc in
            let block = eval_block b in
            let laopt = begin match lao with
                        | Some(a) -> eval_lambda_args a
                        | None -> "" end in
            String.concat "" [spec; " "; block; ".\\ "; laopt] in

        match l with
        | LambdaType (la, b) ->
          let args = eval_lambda_args la in
          let block = eval_block b in
          String.concat "" [args; " "; block] in

    (** [eval_stlc s] evaluates the STLC rule [s] to its corresponding string in LaTeX *)
    let rec eval_stlc (s:Ast.stlc_rule) : string =

      (** [eval_stlc_premises pl l] evaluates each premise in the STLC premise list [pl] and
          appends this result to [l], then concatenates all the strings in this list *)
      let rec eval_stlc_premises (pl:Ast.stlc_premise list) (l:string list) : string =
        match pl with
        | [] -> String.concat "\\hspace{1.5cm}" l
        | h :: t ->
          let prem = begin match h with
                           | STLCPremise (c, b) -> String.concat "" [eval_context c; "\\vdash "; eval_block b]
                           | StrSTLCPremise (c) -> c end in
          eval_stlc_premises t (l @ [prem]) in

      (** [eval_stlc_rule_block srb] evaluates Ast.stlc_rule_block [srb] to the corresponding string in LaTeX *)
      let eval_stlc_rule_block (srb:Ast.stlc_rule_block) : string =
        match srb with
        | STLCRuleBlock (b) -> eval_block b
        | STLCRuleLambda (l) -> eval_lambda l in

      match s with
      | STLCLambda (c, srb) -> String.concat "" [eval_context c; "\\vdash "; eval_stlc_rule_block srb]
      | STLCAxiom (c, srb, n) -> String.concat "" ["\\inferrule*[Right="; n; "]"; "\n{\n\\hspace{1mm}\n}\n{\n"; eval_stlc (STLCLambda (c, srb)); "\n}"]
      | STLCTree (pl, c, srb, n) -> String.concat "" ["\\inferrule*[Right="; n; "]"; "\n{\n"; eval_stlc_premises pl []; "\n}\n{\n"; eval_stlc (STLCLambda (c, srb)); "\n}"]

    (** [eval_context c] evaluates the Ast.context [c] to its corresponding LaTeX string *)
    and eval_context (c:Ast.context) : string =

      (** [eval_block_list bl l] evaluates the list of blocks represented by bl *)
      let rec eval_block_list (bl:Ast.block list) (l:string list) =
          match bl with
          | [] -> l
          | h :: t -> eval_block_list t (l @ [eval_block h]) in

      match c with
      | EmptyContext -> "\\bm{\\cdot}"
      | Gamma -> "\\Gamma"
      | GammaList (bl) -> String.concat "," ("\\Gamma"::(eval_block_list bl [])) in

    (** [eval_sysf s] evaluates the System-F rule [s] to its corresponding string in LaTeX *)
    let rec eval_sysf (s:Ast.sysf_rule) : string =

      (** [eval_sysf_premises pl l] evaluates each premise in the System-F premise list [pl] and
          appends this result to [l], then concatenates all the strings in this list *)
      let rec eval_sysf_premises (pl:Ast.sysf_premise list) (l:string list) : string =
        match pl with
        | [] -> String.concat "\\hspace{1.5cm}" l
        | h :: t ->
          let prem = begin match h with
                           | SystemFPremise (tc, c, b) -> String.concat "" [eval_type_context tc; ","; eval_context c; "\\vdash "; eval_block b]
                           | StrSystemFPremise (c) -> c end in
          eval_sysf_premises t (l @ [prem]) in
      
      (** [eval_sysf_rule_block srb] evaluates Ast.sysf_rule_block [srb] to the corresponding string in LaTeX *)
      let eval_sysf_rule_block (srb:Ast.sysf_rule_block) =
        match srb with
        | SystemFRuleBlock (b) -> eval_block b
        | SystemFRuleLambda (l) -> eval_lambda l in

      match s with
      | SystemFLambda (tc, c, srb) -> String.concat "" [eval_type_context tc; ","; eval_context c; "\\vdash "; eval_sysf_rule_block srb]
      | SystemFAxiom (tc, c, srb, n) -> String.concat "" ["\\inferrule*[Right="; n; "]"; "\n{\n\\hspace{1mm}\n}\n{\n"; eval_sysf (SystemFLambda(tc, c, srb)); "\n}"]
      | SystemFTree (pl, tc, c, srb, n) -> String.concat "" ["\\inferrule*[Right="; n; "]"; "\n{\n"; eval_sysf_premises pl []; "\n}\n{\n"; eval_sysf (SystemFLambda(tc, c, srb)); "\n}"]

    (** [eval_context tc] evaluates the Ast.type_context [tc] to its corresponding LaTeX string *)
    and eval_type_context (tc:Ast.type_context) : string =

      let rec eval_block_list_union (bl:Ast.block list) (l:string list) = 
        match bl with
        | [] ->
          let flower_bracket_adder (s:string) = String.concat "" ["\\{"; s; "\\}"] in
          List.map flower_bracket_adder l
        | h :: t -> eval_block_list_union t (l @ [eval_block h]) in

      match tc with
      | EmptyTypeContext -> "\\bm{\\cdot}"
      | Delta -> "\\Delta"
      | DeltaUnion (bl) -> String.concat "\\cup " ("\\Delta" :: (eval_block_list_union bl [])) in
    
    (** [eval_infer i] evaluates inference rule [i] to a string*)
    let rec eval_infer (i:Ast.infer) : string =
  
      (** [eval_mapping m] evaluates Ast.mapping [m] to a string *)
      let rec eval_mapping (m:Ast.mapping) : string =

        (** [eval_maptype mt] evaluates Ast.maptype [mt] to a string *)
        let eval_maptype (mt:Ast.maptype) : string =
          match mt with
          | SmallStep -> "\\rightarrow"
          | BigStep -> "\\Downarrow"
          | MultiStep -> "\\rightarrow^*"
          | NotSmallStep -> "\\nrightarrow"
          | NotBigStep -> "\\not\\Downarrow"
          | NotMultiStep -> "\\nrightarrow^*" in
  
        (** [eval_mapping_half mh] evaluates Ast.mapping_half [mh] to a string*)
        let eval_mapping_half (mh:Ast.mapping_half) : string =
          match mh with
          | Pair(d1, b1, b2, d2) -> String.concat "" [(eval_delim d1); " "; (eval_block b1); ","; (eval_block b2); (eval_delim d2)]
          | MapStr(c) -> c in

        match m with
        | StoreMapping (mh1, mt, mh2) ->
          String.concat "" [(eval_mapping_half mh1); (eval_maptype mt); " "; (eval_mapping_half mh2)]
        | Hoare (c1, m, c2) -> String.concat "" [String.concat "" ["\\{"; c1; "\\}"]; (eval_mapping m); String.concat "" ["\\{"; c2; "\\}"]] in
  
      (** [eval_infer_list il l] evaluates Ast.infer list [il] to a list of evaluated inference rules *)
      let rec eval_infer_list (il:Ast.infer list) (l:string list) : string =
        match il with
        | [] -> String.concat "\\hspace{1.5cm}" l
        | h :: t -> eval_infer_list t (l @ [(eval_infer h)]) in

      match i with
      | Str (c) -> c
      | Mapping (m) ->
        let mapping = eval_mapping m in
        mapping
      | Axiom (m, c) ->
        let mapping = eval_mapping m in
        String.concat "" ["\\inferrule*[Right="; c; "]"; "\n{\n\\hspace{1mm}\n}\n{\n"; mapping; "\n}"]
      | Rule (il, m, c) ->
        let premises = eval_infer_list il [] in
        let mapping = eval_mapping m in
        String.concat "" ["\\inferrule*[Right="; c; "]"; "\n{\n"; premises; "\n}\n"; "{\n"; mapping; "\n}"] in

    (** [eval_split s] evaluates the split environment [s] to its corresponding string in LaTeX. *)
    let rec eval_split (s:Ast.split) : string =
      match s with
      | MathEq (me) -> failwith ""
      | MathEqList (me, mel) -> failwith "" in

    match se with
    | Infer (i) -> 
      let infer = eval_infer i in
      out @ ["\\begin{mathpar}"; infer; "\\end{mathpar}"]
    | STLCRule (s) ->
      let stlc = eval_stlc s in
      out @ ["\\begin{mathpar}"; stlc; "\\end{mathpar}"]
    | SystemFRule (s) ->
      let sysf = eval_sysf s in
      out @ ["\\begin{mathpar}"; sysf; "\\end{mathpar}"]
    | Lambda (l) -> 
      let lam = eval_lambda l in
      out @ ["\\begin{equation*}"; lam; "\\end{equation*}"]
    | MathEquation (s) ->
      let split_string = eval_split s in
      out @ ["\\begin{equation}\\begin{split}"; split_string; "\\end{split}\\{equation}"] in

  match e with
  | Equation (se) -> out @ (eval_simple_equation se [])
  | EquationList (se, e) -> out @ (eval_simple_equation se []) @ (eval_equation e [])

(** [eval ast out] evaluates the Ast.environment ast to a list of strings,
    appends it to [out], and returns the result of this*)
and eval (ast:Ast.environment) (out:string list) : string list =
  match ast with
  | Text (text) -> out @ (eval_text text)
  | EquationEnv(e) -> out @ (eval_equation e [])
  | ListEnv (env, rem) -> out @ (eval env []) @ (eval rem [])
  (* | ListEnv (env,rem) -> failwith("Unimplemented") *)
  (* | Equation eq -> failwith("Unimplemented") *)
  (* | Table tbl -> failwith("Unimplemented") *)
  | _ -> failwith("Unimplemented")