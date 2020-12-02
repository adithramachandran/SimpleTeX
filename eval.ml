open Ast
open Printf

exception MetadataException of string

let rec start_eval (file:string) (ast:Ast.environment) : unit =
  let header = [
    "\\documentclass[12pt]{article}";
    "\\usepackage[utf8]{inputenc}"] in
  let (remainder_of_ast, header_with_title) = make_title ast header in
  let base_out = header_with_title @ ["\\begin{document}"; "\\maketitle"; "\\setlength{\\parskip}{1em}"] in
  let out = begin match remainder_of_ast with
                  | None -> base_out
                  | Some(a) -> eval a base_out end in
  let final_out = out @ ("\\end{document}" :: []) in
  let file_name = String.concat "" [(String.sub file 0 (String.index file '.')); ".txt"] in
  let oc = open_out file_name in
  write oc final_out;
  close_out oc;
  print_endline "done!"

and write (oc:out_channel) (list:string list) : unit =
  match list with
  | [] -> ()
  | h :: t -> fprintf oc "%s\n" h; write oc t;


and extract_metadata (m:Ast.metadata) (l:string list) : string list =
  match m with
  | MetadataList(m1, m2) -> (extract_metadata m1 []) @ (extract_metadata m2 [])
  | Author(name) -> [String.concat "" ["\\author{"; name; "}"]]
  | Date(date) -> [String.concat "" ["\\date{"; date; "}"]]
  | Title(title) -> [String.concat "" ["\\title{"; title; "}"]]

and make_title (ast:Ast.environment) (header:string list) : Ast.environment option * string list =
  match ast with
  | Metadata (m) -> (None, header @ (extract_metadata m []))
  | ListEnv(Metadata(m), a) -> (Some(a), header @ (extract_metadata m []))
  | _ -> (None, [])


and eval_text (t:Ast.text) : string list =
  match t with
  | NormalText(c) -> [c]
  | TextList(t1, t2) -> (eval_text t1) @ ["\\par"] @ (eval_text t2)

and eval (ast:Ast.environment) (out:string list) : string list =
  match ast with
  | Text (text) -> out @ (eval_text text)
  (* | ListEnv (env,rem) -> failwith("Unimplemented") *)
  (* | ListSetting (set, rem) -> failwith("Unimplemented") *)
  (* | Equation eq -> failwith("Unimplemented") *)
  (* | Table tbl -> failwith("Unimplemented") *)
  (* | PageStyle (pstyle) -> failwith("Unimplemented") *)
  (* | PageOrient (porient) -> failwith("Unimplemented") *)
  (* | MarginSize (msize) -> failwith("Unimplemented") *)
  (* | FontSize (fsize) -> failwith("Unimplemented") *)
  (* | FontStyle (fstyle) -> failwith("Unimplemented") *)
  (* | Spacing (spacing) -> failwith("Unimplemented") *)
  (* | Letter -> failwith("have to edit string") *)
  (* | A4 -> failwith("have to edit string") *)
  (* | Portrait -> failwith("have to edit string") *)
  (* | Landscape -> failwith("have to edit string") *)
  (* | Arial -> failwith("have to edit string") *)
  (* | Times -> failwith("have to edit string") *)
  (* | Cambria -> failwith("have to edit string") *)
  (* | Single -> failwith("have peeeeeeeeeeeenisto edit string") *)
  (* | OnePointFive -> failwith("have to edit string") *)
  (* | Double -> failwith("have to edit string") *)
  | _ -> failwith("Unimplemented")