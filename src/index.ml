module IntMap = Map.Make (Int)
module StrMap = Map.Make (String)

type posting = {
  tf : int;
  positions : int list;
}

type t = {
  docs : Doc.t IntMap.t;
  inv : posting IntMap.t StrMap.t; 
  next_doc_id : int;
}

let empty = { docs = IntMap.empty; inv = StrMap.empty; next_doc_id = 0 }

let doc_count idx = IntMap.cardinal idx.docs
let term_count idx = StrMap.cardinal idx.inv

let postings_for_term idx term =
  match StrMap.find_opt term idx.inv with
  | Some m -> m
  | None -> IntMap.empty

let add_posting ~pos (popt : posting option) : posting =
  match popt with
  | None -> { tf = 1; positions = [ pos ] }
  | Some p -> { tf = p.tf + 1; positions = pos :: p.positions }

let add_document_tokens (idx : t) ~(path : string) (tokens_with_pos : (string * int) list) : t =
  let doc_id = idx.next_doc_id in
  let token_count = List.length tokens_with_pos in
  let doc = Doc.make ~id:doc_id ~path ~tokens:token_count in
  let inv =
    List.fold_left
      (fun inv (term, pos) ->
        if term = "" then inv
        else
          let docs_map =
            match StrMap.find_opt term inv with
            | Some m -> m
            | None -> IntMap.empty
          in
          let prev = IntMap.find_opt doc_id docs_map in
          let updated = add_posting ~pos prev in
          let docs_map' = IntMap.add doc_id updated docs_map in
          StrMap.add term docs_map' inv)
      idx.inv tokens_with_pos
  in
  { docs = IntMap.add doc_id doc idx.docs; inv; next_doc_id = doc_id + 1 }

let is_hidden name = String.length name > 0 && name.[0] = '.'

let parse_exts (csv : string) : string list =
  csv
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun x -> x <> "")
  |> List.map (fun e -> if String.length e > 0 && e.[0] = '.' then e else "." ^ e)

let has_allowed_ext ~exts path =
  match exts with
  | [] -> true
  | exts ->
      List.exists
        (fun ext ->
          let le = String.length ext in
          let lp = String.length path in
          lp >= le && String.sub path (lp - le) le = ext)
        exts

(* rec тут НЕ нужен *)
let list_files ?(follow_symlinks = false) (root : string) : string list =
  let rec walk acc dir =
    let items = try Sys.readdir dir |> Array.to_list with _ -> [] in
    List.fold_left
      (fun acc name ->
        if is_hidden name then acc
        else
          let path = Filename.concat dir name in
          try
            let st = Unix.lstat path in
            match st.Unix.st_kind with
            | Unix.S_DIR -> walk acc path
            | Unix.S_REG -> path :: acc
            | Unix.S_LNK ->
                if follow_symlinks then (
                  try
                    let st2 = Unix.stat path in
                    match st2.Unix.st_kind with
                    | Unix.S_DIR -> walk acc path
                    | Unix.S_REG -> path :: acc
                    | _ -> acc
                  with _ -> acc)
                else acc
            | _ -> acc
          with _ -> acc)
      acc items
  in
  walk [] root |> List.rev

type build_opts = {
  exts : string list;
  max_bytes : int;
  follow_symlinks : bool;
}

let default_build_opts =
  { exts = parse_exts "txt,md,log,csv"; max_bytes = 5_000_000; follow_symlinks = false }

let build_from_dir ?(opts = default_build_opts) (dir : string) : (t, string) result =
  let files =
    list_files ~follow_symlinks:opts.follow_symlinks dir
    |> List.filter (has_allowed_ext ~exts:opts.exts)
  in
  let rec loop idx = function
    | [] -> Ok idx
    | path :: tl -> (
        match Util.safe_read_file ~max_bytes:opts.max_bytes path with
        | Error (`Too_large _n) -> loop idx tl
        | Error `Binary -> loop idx tl
        | Ok text ->
            let toks = Tokenizer.tokenize_with_positions text in
            let idx' = add_document_tokens idx ~path toks in
            loop idx' tl)
  in
  loop empty files
