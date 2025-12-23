open Fp_search

let usage () =
  output_string stderr
    "fp-search — mini full-text search engine\n\n\
     Usage:\n\
     \tfp-search build  --root DIR --out INDEX [--ext txt,md] [--max-bytes N] [--follow-symlinks]\n\
     \tfp-search search --index INDEX --query Q [--top N] [--snippets]\n\
     \tfp-search repl   --index INDEX\n\
     \tfp-search stats  --index INDEX\n\n\
     Examples:\n\
     \tfp-search build --root ./data --out ./index.bin\n\
     \tfp-search search --index ./index.bin --query 'foo AND \"bar baz\"' --top 10 --snippets\n\
     \tfp-search repl --index ./index.bin\n";
  flush stderr

let get_flag argv name = Array.exists (fun s -> s = name) argv

let get_opt argv name ~default =
  let rec loop i =
    if i + 1 >= Array.length argv then default
    else if argv.(i) = name then argv.(i + 1)
    else loop (i + 1)
  in
  loop 0

let get_opt_int argv name ~default =
  match int_of_string_opt (get_opt argv name ~default:(string_of_int default)) with
  | Some x -> x
  | None -> default

let run_build argv =
  let root = get_opt argv "--root" ~default:"." in
  let out = get_opt argv "--out" ~default:"./index.bin" in
  let ext_csv = get_opt argv "--ext" ~default:"txt,md,log,csv" in
  let max_bytes = get_opt_int argv "--max-bytes" ~default:5_000_000 in
  let follow_symlinks = get_flag argv "--follow-symlinks" in
  let opts : Index.build_opts =
    { exts = Index.parse_exts ext_csv; max_bytes; follow_symlinks }
  in
  match Index.build_from_dir ~opts root with
  | Error e ->
      Printf.eprintf "Build error: %s\n%!" e;
      exit 2
  | Ok idx -> (
      match Store.save out idx with
      | Error e ->
          Printf.eprintf "Save error: %s\n%!" e;
          exit 2
      | Ok () ->
          Printf.printf "Indexed %d docs, %d terms -> %s\n%!"
            (Index.doc_count idx) (Index.term_count idx) out)

let load_index_or_exit path =
  match Store.load path with
  | Ok idx -> idx
  | Error e ->
      Printf.eprintf "Load error: %s\n%!" e;
      exit 2

let with_index argv f =
  let path = get_opt argv "--index" ~default:"./index.bin" in
  let idx = load_index_or_exit path in
  f ~path idx

let run_stats argv =
  with_index argv (fun ~path idx ->
      Printf.printf "Index: %s\nDocs: %d\nTerms: %d\n%!" path
        (Index.doc_count idx) (Index.term_count idx))

let run_search argv =
  let path = get_opt argv "--index" ~default:"./index.bin" in
  let query = get_opt argv "--query" ~default:"" in
  let top = get_opt_int argv "--top" ~default:10 in
  let snippets = get_flag argv "--snippets" in
  if query = "" then (
    Printf.eprintf "Missing --query\n%!";
    exit 2);
  let idx = load_index_or_exit path in
  match Query.parse query with
  | Error e ->
      Printf.eprintf "Query parse error: %s\n%!" e;
      exit 2
  | Ok q ->
      let hits = Engine.search idx ~q ~top ~with_snippets:snippets in
      if hits = [] then Printf.printf "(no results)\n%!"
      else
        List.iteri
          (fun i h ->
            Printf.printf "%2d) %s\n%!" (i + 1) (Engine.pp_hit h))
          hits

let run_repl argv =
  with_index argv (fun ~path:_ idx ->
      Printf.printf "fp-search REPL. Type :quit to exit.\nType :help for help.\n%!";
      let rec loop () =
        Printf.printf "> %!";
        match read_line () with
        | exception End_of_file -> ()
        | line ->
            let line = String.trim line in
            if line = "" then loop ()
            else if line = ":quit" || line = ":q" || line = "quit" then ()
            else if line = ":help" then (
              Printf.printf
                "Query syntax: AND/OR/NOT, parentheses, and phrases in quotes.\n\
                 Examples:\n\
                 - foo bar\n\
                 - (foo OR bar) AND baz\n\
                 - NOT deprecated AND \"hello world\"\n%!";
              loop ())
            else (
              match Query.parse line with
              | Error e ->
                  Printf.printf "Parse error: %s\n%!" e;
                  loop ()
              | Ok q ->
                  let hits = Engine.search idx ~q ~top:10 ~with_snippets:true in
                  if hits = [] then Printf.printf "(no results)\n%!"
                  else
                    List.iteri
                      (fun i h ->
                        Printf.printf "%2d) %s\n%!" (i + 1) (Engine.pp_hit h))
                      hits;
                  loop ())
      in
      loop ())

let () =
  if Array.length Sys.argv < 2 then (
    usage ();
    exit 2);
  let cmd = Sys.argv.(1) in
  match cmd with
  | "build" -> run_build Sys.argv
  | "search" -> run_search Sys.argv
  | "repl" -> run_repl Sys.argv
  | "stats" -> run_stats Sys.argv
  | "-h" | "--help" | "help" ->
      usage ();
      exit 0
  | _ ->
      Printf.eprintf "Unknown command: %s\n%!" cmd;
      usage ();
      exit 2
