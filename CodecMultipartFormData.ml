open Batteries

(* Most of this is stolen from ocaml-cgi *)

(* parse_multipart_args: parsing of the CGI arguments for multipart/form-data
   encoding *)

let boundary_re1 =
  Str.regexp_case_fold "boundary=\"\\([^\"]+\\)\""
let boundary_re2 =
  Str.regexp_case_fold "boundary=\\([^ \t\r\n]+\\)"
let name_re1 =
  Str.regexp_case_fold "name=\"\\([^\"]+\\)\""
let name_re2 =
  Str.regexp_case_fold "name=\\([^ \t\r\n;:]+\\)"
let filename_re1 =
  Str.regexp_case_fold "filename=\"\\([^\"]*\\)\""
let filename_re2 =
  Str.regexp_case_fold "filename=\\([^ \t\r\n;:]+\\)"
let content_type_re1 =
  Str.regexp_case_fold "Content-type:[ \t]*\"\\([^\"]+\\)\""
let content_type_re2 =
  Str.regexp_case_fold "Content-type:[ \t]*\\([^ \t\r\n;:]+\\)"
let separator_re =
  Str.regexp "\r\n\r\n"

let match_string re1 re2 str =
  (try ignore (Str.search_forward re1 str 0)
   with Not_found ->
       ignore (Str.search_forward re2 str 0)) ;
  Str.matched_group 1 str

(* Extract field name and value from a chunk.  Raise Not_found if not
   a valid chunk. *)

type field_data =
  { value: string ;
    filename: string ;
    content_type: string }

let field_data_of_text s =
  { value = s ; filename = "" ; content_type = "text/plain" }

let extract_field chunk =
  let pos_separator = Str.search_forward separator_re chunk 0 in
  let header = String.sub chunk 0 pos_separator in
  let field_name =
    match_string name_re1 name_re2 header in
  let filename =
    try match_string filename_re1 filename_re2 header
    with Not_found -> "" in
  let content_type =
    try match_string content_type_re1 content_type_re2 header
    with Not_found -> "" in
  let beg_value = pos_separator + 4 in
  (* Chop final \r\n that browsers insist on putting *)
  let end_value =
    let len = String.length chunk in
    if len >= beg_value && chunk.[len-2] = '\r' && chunk.[len-1] = '\n'
    then len - 2 else len in
  let value =
    String.sub chunk beg_value (end_value - beg_value) in
  (field_name, { filename ; content_type ; value })

(* Same, for a list of chunks *)

let rec extract_fields accu = function
  | [] -> accu
  | chunk :: rem ->
      extract_fields
	      (try extract_field chunk :: accu with Not_found -> accu)
	      rem

module Url =
struct
  (*$< Url *)
  let is_in_set set c = try ignore (String.index set c); true with Not_found -> false

  let reserved_chars = "!*'();:@&=+$,/?#[]"
  let unreseved_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~"
  let is_reserved = is_in_set reserved_chars
  let is_unreserved = is_in_set unreseved_chars

  let int_of_hexchar c_ =
      let c = Char.code c_ in
      if c >= Char.code '0' && c <= Char.code '9' then c - Char.code '0' else
      if c >= Char.code 'a' && c <= Char.code 'f' then 10 + c - Char.code 'a' else
      if c >= Char.code 'A' && c <= Char.code 'F' then 10 + c - Char.code 'A' else (
          Printf.fprintf stderr "Tools: Char is not hex: '%c'\n" c_ ;
          invalid_arg "Bad char"
      )

  (** [decode str] will decode every URL encoded char present in str *)
  let decode s =
      let len = String.length s in
      let s' = Bytes.create len in
      let rec aux o o' =
          if o < len then (
              let skip = ref 1 in
              if o < len - 2 && s.[o] = '%' then (
                  skip := 3 ;
                  let c =
                      try (int_of_hexchar s.[o+1] lsl 4) + int_of_hexchar s.[o+2]
                      with Invalid_argument _ -> Char.code '?' in
                  s'.[o'] <- Char.chr c
              ) else (
                  s'.[o'] <- s.[o]
              ) ;
              aux (o + !skip) (o'+1)
          ) else o' in
      let len' = aux 0 0 in
      let res = Bytes.sub s' 0 len' |> Bytes.to_string in
      (* Printf.printf "Url: decode: '%s' -> '%s'\n" s res ; *)
      res
  (*$= decode & ~printer:identity
      "came_from=/" (decode "came_from=%2F")
  *)
  (*$>*)
end

let parse_multipart_args mime_type body =
  (if String.starts_with mime_type "application/x-www-form-urlencoded" then
    (String.split_on_char '&' body |> List.enum) //@
    (fun nev ->
      match String.split ~by:"=" nev with
      | exception Not_found -> None
      | n, v -> Some (n, field_data_of_text (Url.decode v)))
  else if String.starts_with mime_type "multipart/form-data" then
    (* Determine boundary delimiter *)
    let boundary = "--"^
      try
        match_string boundary_re1 boundary_re2 mime_type
      with Not_found ->
        failwith ("parse_multipart_args: no boundary provided in "^
                  mime_type) in
    String.nsplit ~by:boundary body |>
    extract_fields [] |>
    List.enum
  else
    failwith ("parse_multipart_args: cannot handle MIME type "^
              mime_type)) |>
    Hashtbl.of_enum

(*$= parse_multipart_args & ~printer:dump
  [ "a", { filename = "" ; content_type = "" ; value = "b" } ; \
    "c", { filename = "" ; content_type = "" ; value = "d" } ; \
    "upload", { filename = "testfile" ; \
                content_type = "application/octet-stream" ; \
                value = "testfilecontent\r\n" } ] \
    (parse_multipart_args \
      "multipart/form-data; boundary=------------------------1605451f456c9a1a" \
      "--------------------------1605451f456c9a1a\r\n\\
       Content-Disposition: form-data; name=\"a\"\r\n\\
       \r\n\\
       b\r\n\\
       --------------------------1605451f456c9a1a\r\n\\
       Content-Disposition: form-data; name=\"c\"\r\n\\
       \r\n\\
       d\r\n\\
       --------------------------1605451f456c9a1a\r\n\\
       Content-Disposition: form-data; name=\"upload\"; filename=\"testfile\"\r\n\\
       Content-Type: application/octet-stream\r\n\\
       \r\n\\
       testfilecontent\r\n\\
       \r\n\\
       --------------------------1605451f456c9a1a--" |> \
       Hashtbl.to_list |> List.sort (fun (a,_) (b,_) -> compare a b))
*)
