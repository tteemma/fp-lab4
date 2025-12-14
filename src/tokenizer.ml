(* Tokenizer: splits text into lowercase word tokens and provides token positions (0..n-1). *)

let is_word_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let tokenize_with_positions (s : string) : (string * int) list =
  let buf = Buffer.create 32 in
  let push_token acc pos =
    if Buffer.length buf = 0 then (acc, pos)
    else
      let tok = Buffer.contents buf |> String.lowercase_ascii in
      Buffer.clear buf;
      ((tok, pos) :: acc, pos + 1)
  in
  let rec loop i pos acc =
    if i >= String.length s then
      let acc, _pos = push_token acc pos in
      List.rev acc
    else
      let c = s.[i] in
      if is_word_char c then (
        Buffer.add_char buf c;
        loop (i + 1) pos acc)
      else
        let acc, pos = push_token acc pos in
        loop (i + 1) pos acc
  in
  loop 0 0 []

let tokenize (s : string) : string list = tokenize_with_positions s |> List.map fst

let tokenize_phrase (s : string) : string list =
  tokenize s |> List.filter (fun t -> t <> "")
