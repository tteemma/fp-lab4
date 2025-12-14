module IntSet = Set.Make (Int)

module StringExt = struct
  let contains_str (hay : string) (needle : string) : bool =
    if needle = "" then true
    else
      let lh = String.length hay and ln = String.length needle in
      let rec at i =
        if i + ln > lh then false
        else if String.sub hay i ln = needle then true
        else at (i + 1)
      in
      at 0
end

type hit = {
  doc : Doc.t;
  score : float;
  snippet : string option;
}

let idf ~n_docs ~df = log ((float_of_int (n_docs + 1)) /. float_of_int (df + 1)) +. 1.0
let tf_weight tf = 1.0 +. log (float_of_int (max 1 tf))

let docs_universe (idx : Index.t) : IntSet.t =
  Index.IntMap.fold (fun doc_id _ acc -> IntSet.add doc_id acc) idx.docs IntSet.empty

let term_docs (idx : Index.t) (term : string) : IntSet.t =
  let postings = Index.postings_for_term idx term in
  Index.IntMap.fold (fun doc_id _ acc -> IntSet.add doc_id acc) postings IntSet.empty

let phrase_docs (idx : Index.t) (terms : string list) : IntSet.t =
  let candidate =
    match terms with
    | [] -> IntSet.empty
    | t :: tl ->
        let base = term_docs idx t in
        List.fold_left (fun acc w -> IntSet.inter acc (term_docs idx w)) base tl
  in
  let has_phrase_in_doc doc_id =
    let sets =
      List.map
        (fun w ->
          match Index.postings_for_term idx w |> Index.IntMap.find_opt doc_id with
          | None -> IntSet.empty
          | Some p -> List.fold_left (fun s x -> IntSet.add x s) IntSet.empty p.positions)
        terms
    in
    match sets with
    | [] -> false
    | first :: rest ->
        IntSet.exists
          (fun p0 ->
            let rec ok i = function
              | [] -> true
              | s :: tl -> if IntSet.mem (p0 + i) s then ok (i + 1) tl else false
            in
            ok 1 rest)
          first
  in
  IntSet.filter has_phrase_in_doc candidate

let rec eval_set (idx : Index.t) (q : Query.t) : IntSet.t =
  match q with
  | Query.QTerm w -> term_docs idx w
  | Query.QPhrase ws -> phrase_docs idx ws
  | Query.QAnd (a, b) -> IntSet.inter (eval_set idx a) (eval_set idx b)
  | Query.QOr (a, b) -> IntSet.union (eval_set idx a) (eval_set idx b)
  | Query.QNot a -> IntSet.diff (docs_universe idx) (eval_set idx a)

let collect_terms (q : Query.t) : string list * (string list) list =
  let rec go q (terms, phrases) =
    match q with
    | Query.QTerm w -> (w :: terms, phrases)
    | Query.QPhrase ws -> (terms, ws :: phrases)
    | Query.QAnd (a, b) | Query.QOr (a, b) ->
        let t1, p1 = go a (terms, phrases) in
        go b (t1, p1)
    | Query.QNot a -> go a (terms, phrases)
  in
  go q ([], [])

let score_doc (idx : Index.t) ~(q : Query.t) ~(doc_id : int) : float =
  let n_docs = Index.doc_count idx in
  let terms, phrases = collect_terms q in
  let uniq_terms = terms |> List.filter (fun w -> w <> "") |> List.sort_uniq String.compare in
  let tfidf =
    List.fold_left
      (fun acc term ->
        let postings = Index.postings_for_term idx term in
        match Index.IntMap.find_opt doc_id postings with
        | None -> acc
        | Some p ->
            let df = Index.IntMap.cardinal postings in
            acc +. (tf_weight p.tf *. idf ~n_docs ~df))
      0.0 uniq_terms
  in
  let phrase_bonus =
    List.fold_left
      (fun acc ws -> if IntSet.mem doc_id (phrase_docs idx ws) then acc +. 2.0 else acc)
      0.0 phrases
  in
  tfidf +. phrase_bonus

let snippet_for_doc ~(max_len : int) ~(terms : string list) (path : string) : string option =
  let terms = terms |> List.filter (fun w -> w <> "") |> List.sort_uniq String.compare in
  if terms = [] then None
  else
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop () =
            match input_line ic with
            | line ->
                let low = String.lowercase_ascii line in
                if List.exists (fun t -> StringExt.contains_str low t) terms then Some line else loop ()
            | exception End_of_file -> None
          in
          match loop () with
          | None -> None
          | Some line ->
              if String.length line <= max_len then Some line
              else Some (String.sub line 0 max_len ^ "…"))
    with _ -> None

let search (idx : Index.t) ~(q : Query.t) ~(top : int) ~(with_snippets : bool) : hit list =
  let set = eval_set idx q in
  let terms, _ = collect_terms q in
  let hits =
    IntSet.fold
      (fun doc_id acc ->
        match Index.IntMap.find_opt doc_id idx.docs with
        | None -> acc
        | Some doc ->
            let score = score_doc idx ~q ~doc_id in
            let snippet =
              if with_snippets then snippet_for_doc ~max_len:160 ~terms doc.path else None
            in
            { doc; score; snippet } :: acc)
      set []
  in
  let hits = List.sort (fun a b -> Float.compare b.score a.score) hits in
  let rec take n xs =
    match (n, xs) with
    | 0, _ | _, [] -> []
    | n, x :: tl -> x :: take (n - 1) tl
  in
  take top hits

let pp_hit (h : hit) : string =
  let base = Printf.sprintf "%.4f  %s" h.score h.doc.path in
  match h.snippet with
  | None -> base
  | Some s -> base ^ "\n    " ^ s
