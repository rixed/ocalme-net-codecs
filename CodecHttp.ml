open Batteries

(** {2 HTTP types} *)

type code = int

let text_of_code (c : code) = match c with
    100 -> "Continue"
  | 101 -> "Switching Protocols"
  | 102 -> "Processing"
  | 122 -> "Request-URI too long"
  | 200 -> "OK"
  | 201 -> "Created"
  | 202 -> "Accepted"
  | 203 -> "Non-Authoritative Information"
  | 204 -> "No Content"
  | 205 -> "Reset Content"
  | 206 -> "Partial Content"
  | 207 -> "Multi-Status"
  | 226 -> "IM Used"
  | 300 -> "Multiple Choices"
  | 301 -> "Moved Permanently"
  | 302 -> "Found"
  | 303 -> "See Other"
  | 304 -> "Not Modified"
  | 305 -> "Use Proxy"
  | 306 -> "Switch Proxy"
  | 307 -> "Temporary Redirect"
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 402 -> "Payment Required"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 406 -> "Not Acceptable"
  | 407 -> "Proxy Authentication Required"
  | 408 -> "Request Timeout"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length Required"
  | 412 -> "Precondition Failed"
  | 413 -> "Request Entity Too Large"
  | 414 -> "Request-URI Too Long"
  | 415 -> "Unsupported Media Type"
  | 416 -> "Requested Range Not Satisfiable"
  | 417 -> "Expectation Failed"
  | 418 -> "I'm a teapot"
  | 422 -> "Unprocessable Entity"
  | 423 -> "Locked"
  | 424 -> "Failed Dependency"
  | 425 -> "Unordered Collection"
  | 426 -> "Upgrade Required"
  | 444 -> "No Response"
  | 449 -> "Retry With"
  | 450 -> "Blocked by Windows Parental Controls"
  | 499 -> "Client Closed Request"
  | 500 -> "Internal Server Error"
  | 501 -> "Not Implemented"
  | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable"
  | 504 -> "Gateway Timeout"
  | 505 -> "HTTP Version Not Supported"
  | 506 -> "Variant Also Negotiates"
  | 507 -> "Insufficient Storage"
  | 509 -> "Bandwidth Limit Exceeded"
  | 510 -> "Not Extended"
  | _   -> "Unknown HTTP Error"

module Command = struct
  type t = GET | HEAD | POST | PUT | DELETE | CONNECT
         | OPTIONS | TRACE | PATCH | Other of string

  let to_string = function
    | GET -> "GET"
    | HEAD -> "HEAD"
    | POST -> "POST"
    | PUT -> "PUT"
    | DELETE -> "DELETE"
    | CONNECT -> "CONNECT"
    | OPTIONS -> "OPTIONS"
    | TRACE -> "TRACE"
    | PATCH -> "PATCH"
    | Other s -> s

  let of_string s =
    match String.uppercase s with
    | "GET" -> GET
    | "HEAD" -> HEAD
    | "POST" -> POST
    | "PUT" -> PUT
    | "DELETE" -> DELETE
    | "CONNECT" -> CONNECT
    | "OPTIONS" -> OPTIONS
    | "TRACE" -> TRACE
    | "PATCH" -> PATCH
    | _ -> Other s

  let print fmt t = String.print fmt (to_string t)
end

module Version = struct
  type t = int * int
  let print fmt (mi,ma) =
    Printf.fprintf fmt "%d.%d" mi ma
end

module RequestLine = struct
  type t = { cmd : Command.t ; url : string ; version : Version.t }
  let print fmt t =
    Printf.fprintf fmt "%a %s HTTP/%a"
      Command.print t.cmd t.url Version.print t.version
end

module StatusLine = struct
  type t = { version : Version.t ; code : code ; msg : string }
  let print fmt t =
    Printf.fprintf fmt "HTTP/%a %d %s" Version.print t.version t.code t.msg
end

module StartLine = struct
  type t = Request of RequestLine.t | Response of StatusLine.t
  let print fmt = function
    | Request r  -> RequestLine.print fmt r
    | Response r -> StatusLine.print fmt r
end

module Header = struct
  type t = string * string
  let print fmt (n, v) =
    Printf.fprintf fmt "%s: %s" n v
end

module Headers = struct
  (* TODO: parse some well known headers that are likely to
   * be used by client or needed by the body parser. *)
  type t = Header.t list
  let print nl fmt = function
    | [] -> () (* must not insert a newline *)
    | hs ->
      (List.print ~first:"" ~last:nl ~sep:nl
        Header.print) fmt hs
end

module Body = struct
  type t = string
  let print fmt = String.print fmt
end

module Msg = struct
  type t = {
    start_line : StartLine.t ;
    headers : Headers.t ;
    body : Body.t }

  let print fmt t =
    Printf.fprintf fmt "%a\n%a\n%s\n"
      StartLine.print t.start_line
      (Headers.print "\n") t.headers
      t.body

  let encode t =
    Printf.sprintf2 "%a\r\n%a\r\n%s"
      StartLine.print t.start_line
      (Headers.print "\r\n") t.headers
      t.body
end

let rec headers_find f = function
    | [] -> None
    | (f', v) :: hs' ->
        if String.icompare f' f = 0 then Some v else headers_find f hs'

let headers_find_all f hs =
    let matching_h = List.filter (fun (f', _) -> String.icompare f' f = 0) hs in
    List.map snd matching_h

let header_present n v hs = match headers_find n hs with
    | Some str when String.icompare str v = 0 -> true
    | _ -> false

let must_close_cnx = header_present "Connection" "close"

(** {2 HTTP Messages} *)

(* those types are bogus ; use the types that are output from the parser.
 * We want those types to be common for encoder/decoder, *BUT* we want encode and
 * decode to be in two different modules so that a program do not have to have both.
 * Esp. only decoders should depend on parsercombinator. *)

module MakeParser (Conf : Parsers.CONFIG with type token = char) =
struct
  module P = Parsers.Make (Conf)
  open P
  module U = ParsersUsual.Make (P)
  open U

  let unsigned_int ?what =
    unsigned_decimal_number ?what >>: Num.int_of_num

  let version m =
    let m = "HTTP version"::m in
    (string "HTTP/" -+ unsigned_int +-
     item '.' ++ unsigned_int) m

  (*$= & ~printer:(IO.to_string (print_result Version.print))
    (Ok (1, 0)) \
      (let open HttpParser in \
       let open P in \
       (version +- eof) [] None no_corr \
         (ParserConfig.stream_of_string "HTTP/1.0") |> \
       to_result_no_stream)
   *)

  let spaces m =
    let m = "spaces"::m in
    several_greedy ~sep:none (item ' ') m
  let maybe_spaces m =
    let m = "optional spaces"::m in
    repeat_greedy ~sep:none (item ' ') m
  let non_spaces ?what ?max m =
    let m = "non spaces"::m in
    let not_a_space c = c <> ' ' && c <> '\r' && c <> '\n' in
    several_greedy ?what ~sep:none ?max
                   (cond "non-space" not_a_space 'x') m
  let crlf m =
    let m = "end of line"::m in
    (carriage_return -- new_line) m

  let request_line m =
    let m = "HTTP request line"::m in
    (* TODO: with_context "HTTP command" (string "GET" || string "HEAD" ...) *)
    ((non_spaces ~max:12 ~what:"HTTP command" >>: String.of_list) +-
     spaces ++
     (non_spaces ~max:10000 ~what:"URL" >>: String.of_list) +-
     spaces ++
     version +-
     crlf >>:
     fun ((cmd, url), version) ->
       RequestLine.{ cmd = Command.of_string cmd ; url ; version }
    ) m

  (*$= & ~printer:(IO.to_string (print_result RequestLine.print))
    (Ok RequestLine.{ cmd = Command.GET ; url = "/test1" ; version = 1, 0 }) \
      (let open HttpParser in \
       let open P in \
       (request_line +- eof) [] None no_corr (ParserConfig.stream_of_string \
         "GET /test1 HTTP/1.0\r\n") |> \
       to_result_no_stream)
   *)

  let status_line  m =
    let m = "HTTP status line"::m in
    (version +-
     spaces ++
     unsigned_int ~what:"HTTP status code" +-
     spaces ++
     (several_greedy ~what:"HTTP status message" ~sep:none
       (cond "no carriage-return" (fun c -> c <> '\r') 'x') >>:
         String.of_list) +-
     crlf >>:
     fun ((version, code), msg) ->
       StatusLine.{ version ; code ; msg }
    ) m

  (*$= & ~printer:(IO.to_string (print_result StatusLine.print))
    (Ok StatusLine.{ version = (2, 1) ; code = 200 ; msg = "OK" }) \
      (let open HttpParser in \
       let open P in \
       (status_line +- eof) [] None no_corr (ParserConfig.stream_of_string \
         "HTTP/2.1 200 OK\r\n") |> \
       to_result_no_stream)
   *)

  let start_line m =
    ((request_line >>: fun r -> StartLine.Request r) |||
     (status_line  >>: fun r -> StartLine.Response r)) m

  let header m =
    let m = "HTTP header"::m in
    let field_value_char c =
      c <> '\r' && c <> '\n' in
    let field_name_char c =
      c <> ':' && c <> ' ' && c <> '\t' &&
      field_value_char c in
    ((several_greedy ~sep:none ~what:"field name"
        (cond "field name char" field_name_char 'x') >>: 
      String.of_list) +-
     item ':' +- maybe_spaces ++
     (* TODO: multi-line values *)
     (several_greedy ~sep:none ~what:"field value"
        (cond "field value char" field_value_char 'x') >>:
      fun l -> String.strip ~chars:"\t\r\n " (String.of_list l)) +-
    crlf) m

  (*$= & ~printer:(IO.to_string (print_result Header.print))
    (Ok ("GlopGlop", "pas glop")) \
      (let open HttpParser in \
       let open P in \
       (header +- eof) [] None no_corr (ParserConfig.stream_of_string \
        "GlopGlop: pas glop\r\n") |> \
       to_result_no_stream)
    (* No space after ':' is OK: *) \
    (Ok ("Content-Length", "42")) \
      (let open HttpParser in \
       let open P in \
        (header +- eof) [] None no_corr (ParserConfig.stream_of_string \
          "Content-Length:42\r\n") |> \
        to_result_no_stream)
   *)

  let headers m =
    (repeat_greedy ~sep:none ~what:"HTTP headers" header +-
     crlf) m

  let chunk_header m =
    ((non_decimal_integer 16 none hexadecimal_digit >>: Num.int_of_num) +-
     crlf) m

  let chunk m =
    ((chunk_header >>= fun sz -> times sz ~sep:none ~what:"chunk" anything) +-
     crlf) m

  let chunks m =
    (several_greedy ~sep:none ~what:"body chunks" chunk >>:
     List.concat) m

  (* We take the start line and headers so that we can use the info
   * provided here. *)
  let body sl hs m =
    let m = "HTTP body"::m in
    (* "Any response message which "MUST NOT" include a message-body (such as the 1xx,
       204, and 304 responses and any response to a HEAD request) is always terminated
       by the first empty line after the header fields, regardless of the entity-header
       fields present in the message."
        - http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.4.
       TODO: for responses to HEAD, we do not handle this case yet. *)
    ((match sl with
    | StartLine.Response r
      when StatusLine.(r.code >= 100 && r.code <= 199 ||
                       r.code = 204 || r.code = 304) ->
      (* responses required to have no body *)
      return []
    | StartLine.Request { RequestLine.cmd = Command.GET ; _ } ->
      (* requests required to have no body *)
      return []
    | _ -> (match headers_find "Transfer-Encoding" hs with
      (* FIXME: when te _contains_ "chunked"? *)
      | Some te when String.icompare te "chunked" = 0 ->
        chunks
      | _ -> (match headers_find "Content-Length" hs with
        | Some cl ->
          let cl = int_of_string cl in
          times cl ~sep:none ~what:"fixed length body" anything
        | None ->
          repeat_greedy ~sep:none ~what:"body" anything
      )
    )) >>: String.of_list) m

  let p m =
    (optional_greedy ~def:() crlf -+
     start_line ++
     headers >>= fun (sl, hs) ->
     body sl hs >>: fun body ->
     Msg.{ start_line = sl ; headers = hs ; body }) m

  (*$= & ~printer:(IO.to_string (print_result Msg.print))
    (* No headers is OK: *) \
    (Ok Msg.{ \
      start_line = StartLine.Request RequestLine.{ cmd = Command.GET ; url = "/" ; version = 1,0 } ; \
      headers = [] ; \
      body = "" }) \
      (HttpParser.p [] None no_corr \
         (ParserConfig.stream_of_string "GET / HTTP/1.0\r\n\r\n") |> \
       to_result_no_stream)
    (* Trailing garbage must not prevent the parser to find the message: *) \
    (Ok Msg.{ \
      start_line = StartLine.Request RequestLine.{ cmd = Command.GET ; url = "/" ; version = 1,0 } ; \
      headers = [ "Hello", "World" ] ; \
      body = "" }) \
      (HttpParser.p [] None no_corr \
         (ParserConfig.stream_of_string "GET / HTTP/1.0\r\nHello: World\r\n\r\nXXX") |> \
       to_result_no_stream)
    (* ... even when there are no headers: *) \
    (Ok Msg.{ \
      start_line = StartLine.Request RequestLine.{ cmd = Command.GET ; url = "/" ; version = 1,0 } ; \
      headers = [] ; \
      body = "" }) \
      (HttpParser.p [] None no_corr \
         (ParserConfig.stream_of_string "GET / HTTP/1.0\r\n\r\nXXX") |> \
       to_result_no_stream)
    (Ok test_files.(0).pdu) \
      (HttpParser.p [] None no_corr \
         (stream_of_file test_files.(0).fname) |> \
       to_result_no_stream |> abbreviate_result)
    (Ok test_files.(1).pdu) \
      (HttpParser.p [] None no_corr \
         (stream_of_file test_files.(1).fname) |> \
       to_result_no_stream |> abbreviate_result)
    (Ok test_files.(2).pdu) \
      (HttpParser.p [] None no_corr \
         (stream_of_file test_files.(2).fname) |> \
       to_result_no_stream |> abbreviate_result)
   *)
end

(*
let post_decode body =
  Url.decode (String.nreplace ~str:body ~sub:"+" ~by:" ")

let post_encode body =
  String.nreplace ~str:(Url.encode body) ~sub:" " ~by:"+"
*)
