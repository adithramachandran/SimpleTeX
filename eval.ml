open Ast
open Printf

let rec start_eval (file:string) (ast:Ast.environment) : unit =
  let base_out = [
    "\\documentclass{article}";
    "\\usepackage[utf8]{inputenc}";
    "\\title{sample latex file}";
    "\\author{author: adith, atul}";
    "\\date{date: today's date}";
    "\\begin{document}";
    "\\maketitle"] in
  let final_out = eval ast base_out in
  let final_final_out = final_out @ ("\\end{document}" :: []) in
  let oc = open_out "simpletex.txt" in
  write oc final_final_out;
  close_out oc;
  print_endline "done!"

and write (oc:out_channel) (list:string list) : unit =
  match list with
  | [] -> ()
  | h :: t -> fprintf oc "%s\n" h; write oc t;

and eval (ast:Ast.environment) (out:string list) : string list =
  match ast with
  | Text (text) ->
    begin match text with
    | NormalText (content) -> out @ (content :: []) end
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