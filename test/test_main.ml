open Alcotest
open Fp_search

let ( // ) = Filename.concat

let write_file path content =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

let rec rm_rf path =
  if Sys.file_exists path then
    if Sys.is_directory path then (
      Sys.readdir path
      |> Array.iter (fun name -> rm_rf (path // name));
      Unix.rmdir path)
    else Sys.remove path

let with_temp_dir f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    base
    // ("fp_search_test_" ^ string_of_int (Unix.getpid ())
       ^ "_" ^ string_of_int (Random.int 1_000_000))
  in
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)


let ends_with ~suffix s =
  let ls = String.length suffix in
  let l = String.length s in
  l >= ls && String.sub s (l - ls) ls = suffix


let util_tests =
  [
    test_case "split_on_char" `Quick (fun () ->
        check (list string) "basic"
          [ "a"; "b"; "c" ]
          (Util.split_on_char ',' "a,b,c"));

    test_case "trim" `Quick (fun () ->
        check string "trim"
          "hello"
          (Util.trim " \n hello \t "));

    test_case "safe_read_file: ok / binary / too_large" `Quick (fun () ->
        with_temp_dir (fun dir ->
            let ok = dir // "a.txt" in
            let bin = dir // "b.txt" in
            let big = dir // "c.txt" in
            write_file ok "hello";
            write_file bin "a\000b";
            write_file big (String.make 100 'x');

            (match Util.safe_read_file ~max_bytes:10 ok with
            | Ok _ -> ()
            | Error e ->
                Alcotest.failf "expected Ok, got Error %s"
                  (match e with
                  | `Binary -> "Binary"
                  | `Too_large n -> "Too_large(" ^ string_of_int n ^ ")"));

            (match Util.safe_read_file ~max_bytes:10 bin with
            | Error `Binary -> ()
            | Ok _ -> Alcotest.fail "expected Binary, got Ok"
            | Error _ -> Alcotest.fail "expected Binary");

            (match Util.safe_read_file ~max_bytes:10 big with
            | Error (`Too_large _) -> ()
            | Ok _ -> Alcotest.fail "expected Too_large, got Ok"
            | Error _ -> Alcotest.fail "expected Too_large")));
  ]

let tokenizer_tests =
  [
    test_case "tokenize_with_positions" `Quick (fun () ->
        let toks =
          Tokenizer.tokenize_with_positions
            "Hello, world! Hello again"
        in
        check (list (pair string int))
          "tokens"
          [ ("hello", 0); ("world", 1); ("hello", 2); ("again", 3) ]
          toks);

    test_case "tokenize_phrase drops empties" `Quick (fun () ->
        check (list string) "phrase"
          [ "hello"; "world" ]
          (Tokenizer.tokenize_phrase "  hello   world  "));
  ]

let query_tests =
  let open Query in
  [
    test_case "implicit AND" `Quick (fun () ->
        match Query.parse "foo bar" with
        | Ok (QAnd (QTerm "foo", QTerm "bar")) -> ()
        | _ -> Alcotest.fail "expected implicit AND");

    test_case "AND precedence over OR" `Quick (fun () ->
        match Query.parse "foo OR bar AND baz" with
        | Ok (QOr (_, QAnd _)) -> ()
        | _ -> Alcotest.fail "wrong precedence");

    test_case "NOT binds tight" `Quick (fun () ->
        match Query.parse "NOT foo AND bar" with
        | Ok (QAnd (QNot _, _)) -> ()
        | _ -> Alcotest.fail "NOT precedence broken");

    test_case "phrase in quotes" `Quick (fun () ->
        match Query.parse "\"hello world\"" with
        | Ok (QPhrase [ "hello"; "world" ]) -> ()
        | _ -> Alcotest.fail "phrase not parsed");
  ]

let index_tests =
  [
    test_case "parse_exts" `Quick (fun () ->
        check (list string) "exts"
          [ ".txt"; ".md" ]
          (Index.parse_exts "txt,md"));

    test_case "list_files ignores hidden dirs" `Quick (fun () ->
        with_temp_dir (fun dir ->
            let visible = dir // "a.txt" in
            let hidden = dir // ".hidden" in
            let hidden_file = hidden // "b.txt" in
            Unix.mkdir hidden 0o700;
            write_file visible "x";
            write_file hidden_file "y";

            let files = Index.list_files ~follow_symlinks:false dir in
            check bool "visible present" true (List.mem visible files);
            check bool "hidden ignored" false
              (List.exists (fun p -> ends_with ~suffix:"b.txt" p) files)));

    test_case "build_from_dir skips binary / large / wrong ext" `Quick (fun () ->
        with_temp_dir (fun dir ->
            write_file (dir // "a.txt") "hello";
            write_file (dir // "b.md") "hello hello";
            write_file (dir // "c.bin") "hello";
            write_file (dir // "d.txt") (String.make 200 'x');

            let opts : Index.build_opts =
              {
                exts = Index.parse_exts "txt,md";
                max_bytes = 50;
                follow_symlinks = false;
              }
            in
            match Index.build_from_dir ~opts dir with
            | Ok idx ->
                check int "doc_count" 2 (Index.doc_count idx)
            | Error e -> Alcotest.fail e));
  ]

let engine_store_tests =
  [
    test_case "tf-idf ranks frequent term higher" `Quick (fun () ->
        with_temp_dir (fun dir ->
            write_file (dir // "a.txt") "hello world";
            write_file (dir // "b.txt") "hello hello";

            let idx =
              match Index.build_from_dir dir with
              | Ok i -> i
              | Error e -> Alcotest.fail e
            in
            let q =
              match Query.parse "hello" with
              | Ok q -> q
              | Error e -> Alcotest.fail e
            in
            let hits = Engine.search idx ~q ~top:2 ~with_snippets:false in
            match hits with
            | h1 :: _ ->
                check bool "b.txt first" true
                  (ends_with ~suffix:"b.txt" h1.doc.path)
            | _ -> Alcotest.fail "no hits"));

    test_case "phrase matches consecutive tokens only" `Quick (fun () ->
        with_temp_dir (fun dir ->
            write_file (dir // "a.txt") "hello world";
            write_file (dir // "b.txt") "hello brave world";

            let idx =
              match Index.build_from_dir dir with
              | Ok i -> i
              | Error e -> Alcotest.fail e
            in
            let q =
              match Query.parse "\"hello world\"" with
              | Ok q -> q
              | Error e -> Alcotest.fail e
            in
            let hits = Engine.search idx ~q ~top:10 ~with_snippets:false in
            check int "one hit" 1 (List.length hits)));


    test_case "store save/load roundtrip" `Quick (fun () ->
        with_temp_dir (fun dir ->
            write_file (dir // "a.txt") "hello";

            let idx =
              match Index.build_from_dir dir with
              | Ok i -> i
              | Error e -> Alcotest.fail e
            in
            let path = dir // "idx.bin" in
            match Store.save path idx with
            | Error e -> Alcotest.fail e
            | Ok () -> (
                match Store.load path with
                | Ok idx2 ->
                    check int "doc_count"
                      (Index.doc_count idx)
                      (Index.doc_count idx2)
                | Error e -> Alcotest.fail e)));
  ]

let () =
  Alcotest.run "fp-search"
    [
      ("util", util_tests);
      ("tokenizer", tokenizer_tests);
      ("query", query_tests);
      ("index", index_tests);
      ("engine_store", engine_store_tests);
    ]
