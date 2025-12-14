type t = {
  id : int;
  path : string;
  tokens : int;
}

let make ~id ~path ~tokens = { id; path; tokens }
