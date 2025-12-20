type token =
  | Term of string
  | Phrase of string list
  | And
  | Or
  | Not
  | LParen
  | RParen
  | Eof

type t =
  | QTerm of string
  | QPhrase of string list
  | QAnd of t * t
  | QOr of t * t
  | QNot of t

let is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let tokenize (s : string) : token list =
  let len = String.length s in
  let buf = Buffer.create 32 in
  let flush_term acc =
    if Buffer.length buf = 0 then acc
    else
      let raw = Buffer.contents buf in
      Buffer.clear buf;
      let w = String.lowercase_ascii (String.trim raw) in
      if w = "" then acc
      else if w = "and" then And :: acc
      else if w = "or" then Or :: acc
      else if w = "not" then Not :: acc
      else Term w :: acc
  in

  let read_phrase i acc =
    let i = i + 1 in
    let start = i in
    let rec find_end j =
      if j >= len then (len, false)
      else if s.[j] = '"' then (j, true)
      else find_end (j + 1)
    in
    let j, ok = find_end start in
    let phrase_raw = String.sub s start (j - start) in
    let terms = Tokenizer.tokenize_phrase phrase_raw in
    let acc = Phrase terms :: acc in
    let next_i = if ok then j + 1 else j in
    (next_i, acc)
  in

  let rec loop i acc =
    if i >= len then List.rev (Eof :: flush_term acc)
    else
      let c = s.[i] in
      if is_space c then loop (i + 1) (flush_term acc)
      else if c = '(' then loop (i + 1) (LParen :: flush_term acc)
      else if c = ')' then loop (i + 1) (RParen :: flush_term acc)
      else if c = '"' then
        let acc = flush_term acc in
        let next_i, acc = read_phrase i acc in
        loop next_i acc
      else (
        Buffer.add_char buf c;
        loop (i + 1) acc)
  in
  loop 0 []

type parser_state = { toks : token array; mutable i : int }

let peek st = st.toks.(st.i)
let consume st =
  let t = st.toks.(st.i) in
  st.i <- st.i + 1;
  t

let token_starts_primary = function
  | Term _ | Phrase _ | LParen | Not -> true
  | _ -> false

let rec parse_expr st = parse_or st

and parse_or st =
  let left = parse_and st in
  let rec loop acc =
    match peek st with
    | Or ->
        ignore (consume st);
        let right = parse_and st in
        loop (QOr (acc, right))
    | _ -> acc
  in
  loop left

and parse_and st =
  let left = parse_not st in
  let rec loop acc =
    match peek st with
    | And ->
        ignore (consume st);
        let right = parse_not st in
        loop (QAnd (acc, right))
    | tok when token_starts_primary tok ->
        (* implicit AND: `foo bar` *)
        let right = parse_not st in
        loop (QAnd (acc, right))
    | _ -> acc
  in
  loop left

and parse_not st =
  match peek st with
  | Not ->
      ignore (consume st);
      QNot (parse_not st)
  | _ -> parse_primary st

and parse_primary st =
  match consume st with
  | Term w -> QTerm w
  | Phrase ws -> QPhrase ws
  | LParen ->
      let e = parse_expr st in
      (match consume st with
      | RParen -> e
      | _ -> failwith "Expected ')'")
  | tok ->
      failwith
        (Printf.sprintf "Unexpected token in query: %s"
           (match tok with
           | And -> "AND"
           | Or -> "OR"
           | Not -> "NOT"
           | LParen -> "("
           | RParen -> ")"
           | Eof -> "EOF"
           | Term _ -> "TERM"
           | Phrase _ -> "PHRASE"))

let parse (s : string) : (t, string) result =
  try
    let toks = tokenize s |> Array.of_list in
    let st = { toks; i = 0 } in
    let ast = parse_expr st in
    Ok ast
  with Failure msg -> Error msg

let rec to_string = function
  | QTerm w -> w
  | QPhrase ws -> "\"" ^ String.concat " " ws ^ "\""   (* ВОТ ТУТ фикс кавычек *)
  | QAnd (a, b) -> "(" ^ to_string a ^ " AND " ^ to_string b ^ ")"
  | QOr (a, b) -> "(" ^ to_string a ^ " OR " ^ to_string b ^ ")"
  | QNot a -> "(NOT " ^ to_string a ^ ")"
