let magic = "FP_SEARCH_IDX_V1"

let save (path : string) (idx : Index.t) : (unit, string) result =
  try
    let oc = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc magic;
        Marshal.to_channel oc idx [];
        Ok ())
  with e -> Error (Printexc.to_string e)

let load (path : string) : (Index.t, string) result =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let header = really_input_string ic (String.length magic) in
        if header <> magic then Error "Invalid index file (magic mismatch)"
        else
          let idx : Index.t = Marshal.from_channel ic in
          Ok idx)
  with e -> Error (Printexc.to_string e)
