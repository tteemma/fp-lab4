let starts_with ~prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let ends_with ~suffix s =
  let ls = String.length suffix in
  let l = String.length s in
  l >= ls && String.sub s (l - ls) ls = suffix

let split_on_char c s =
  let rec go i j acc =
    if j = String.length s then
      let part = String.sub s i (j - i) in
      List.rev (part :: acc)
    else if s.[j] = c then
      let part = String.sub s i (j - i) in
      go (j + 1) (j + 1) (part :: acc)
    else
      go i (j + 1) acc
  in
  if s = "" then [] else go 0 0 []

let trim s =
  let is_space = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false in
  let len = String.length s in
  let rec left i =
    if i >= len then len else if is_space s.[i] then left (i + 1) else i
  in
  let rec right i =
    if i < 0 then -1 else if is_space s.[i] then right (i - 1) else i
  in
  let l = left 0 in
  let r = right (len - 1) in
  if r < l then "" else String.sub s l (r - l + 1)

let safe_read_file ~max_bytes path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len =
        try in_channel_length ic with _ ->
          max_bytes + 1
      in
      if len > max_bytes then Error (`Too_large len)
      else
        let data = really_input_string ic len in
        if String.contains data '\000' then Error `Binary else Ok data)
