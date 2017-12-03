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

let parse_multipart_args mime_type body =
  if not (String.starts_with mime_type "multipart/form-data")
  then
    failwith ("parse_multipart_args: cannot handle MIME type "^
              mime_type) ;
  (* Determine boundary delimiter *)
  let boundary = "--"^
    try
      match_string boundary_re1 boundary_re2 mime_type
    with Not_found ->
      failwith ("parse_multipart_args: no boundary provided in "^
                mime_type) in
  String.nsplit ~by:boundary body |>
  extract_fields [] |>
  List.rev

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
       --------------------------1605451f456c9a1a--")
*)
