open Alcotest
open Fp_search

let test_tokenizer_basic () =
  let toks = Tokenizer.tokenize "Hello, world! hello_world 123" in
  check (list string) "tokens" [ "hello"; "world"; "hello_world"; "123" ] toks

let test_query_parse_precedence () =
  (* NOT > AND > OR *)
  let q = "a OR b AND NOT c" in
  match Query.parse q with
  | Error e -> fail e
  | Ok ast ->
      let s = Query.to_string ast in
      check string "pretty" "(a OR (b AND (NOT c)))" s

let test_phrase_tokenize () =
  match Query.parse "\"hello world\" AND test" with
  | Error e -> fail e
  | Ok ast ->
      let s = Query.to_string ast in
      check string "phrase" "(\"hello world\" AND test)" s

let () =
  run "fp_search"
    [
      ("tokenizer", [ test_case "basic" `Quick test_tokenizer_basic ]);
      ( "query",
        [
          test_case "precedence" `Quick test_query_parse_precedence;
          test_case "phrase" `Quick test_phrase_tokenize;
        ] );
    ]
