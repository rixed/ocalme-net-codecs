
open Batteries

(* It is preferable to escape special chars *)
module MyChar = struct
  include Char
  let print fmt c = Printf.fprintf fmt "%C" c
end

module ParserConfig = Parsers.SimpleConfig (MyChar)

let stream_of_string s =
  let rec loop n tl =
    if n < 0 then tl else
    loop (n-1) ((n, s.[n]) :: tl) in
  loop (String.length s - 1) []

(* If I earned one cent each time I wrote that one... *)
let read_whole_file file =
  let ic = Unix.(openfile file [O_RDONLY] 0 |> input_of_descr) in
  IO.read_all ic (* autoclosed *)

let stream_of_file f =
  read_whole_file f |> stream_of_string

let no_corr = Parsers.no_error_correction

open CodecHttp
module HttpParser = MakeParser (ParserConfig)

(* Some test files and content: *)

(* To not have to embed the whole body, we do not compare the body but an
 * abbreviated body: *)
let abbreviated_body body =
  let len = String.length body in
  if len < 100 then body else
    Printf.sprintf "%s... length was %d ...%s"
      (String.sub body 0 10)
      len
      (String.sub body (len-10) 10)

let abbreviate_body pdu =
  Msg.{ pdu with body = abbreviated_body pdu.body }

let abbreviate_result = function
  | Ok pdu -> Ok (abbreviate_body pdu)
  | x -> x

type test_file = { fname : string ; pdu : Msg.t }
let test_files = [|
  { fname = "samples/http.post" ;
    pdu = Msg.{
      start_line = StartLine.Request RequestLine.{
        cmd = Command.POST ; url = "/glop/pas_glop.html" ; version = 1,1 } ;
      headers = [
        "Host", "vsonde99:8080" ;
        "User-Agent", "Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.9.1.16) Gecko/20110323 Iceweasel/3.5.16 (like Firefox/3.5.16)" ;
        "Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" ;
        "Accept-Language", "fr,en-us;q=0.7,en;q=0.3" ;
        "Accept-Encoding", "gzip,deflate" ;
        "Accept-Charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.7" ;
        "Keep-Alive", "300" ;
        "Connection", "keep-alive" ;
        "Referer", "http://vsonde99:8080/nevrax/login.html?came_from=%2F" ;
        "Cookie", "zope3_cs_12a918de=E-T7bb8N2YCP3Pes6vw-.3NHLAgsVicRDtNSioCFUZhc6o0-CfMaF4; auth_tkt=\"\"" ;
        "Content-Type", "application/x-www-form-urlencoded" ;
        "Content-Length", "57" ] ;
      body = "login=admin&password=admin&submit=Connexion&came_from=%2F" } } ;
  { fname = "samples/http.403" ;
    pdu = Msg.{
      start_line = StartLine.Response StatusLine.{
        version = 1,0 ; code = 403 ; msg = "Forbidden" } ;
      headers = [
        "Content-Length", "1207" ;
        "Content-Type", "text/html" ;
        "Date", "Tue, 13 Sep 2011 06:32:49 GMT" ;
        "Server", "GFE/2.0" ] ;
      body = "<html><hea... length was 1207 ...dy></html>" } } ;
  { fname = "samples/http.chunked" ;
    pdu = Msg.{
      start_line = StartLine.Response StatusLine.{
        version = 1,1 ; code = 200 ; msg = "OK" } ;
      headers = [
          "Date", "Tue, 26 Jul 2011 20:22:39 GMT" ;
          "Expires", "-1" ;
          "Cache-Control", "private, max-age=0" ;
          "Content-Type", "text/html; charset=ISO-8859-1" ;
          "Set-Cookie", "PREF=ID=b95829f12a4b86f7:FF=0:TM=1311711759:LM=1311711759:S=9O3aXLBvEVzfWRsu; expires=Thu, 25-Jul-2013 20:22:39 GMT; path=/; domain=.google.fr" ;
          "Set-Cookie", "NID=49=i6seWipLkoquvx19DUzBi_-l3ZvD7tRfOd3Vc7WTAhMOk040PlK7NlA07d49X9IhMXzk5MOHhWciC_oQbE9HWIrT4npZImGdSZBDNcU26Nbr9H5E-noq-F2u-b97hoHC; expires=Wed, 25-Jan-2012 20:22:39 GMT; path=/; domain=.google.fr; HttpOnly" ;
          "Server", "gws" ;
          "X-XSS-Protection", "1; mode=block" ;
          "Transfer-Encoding", "chunked" ] ;
      body = "<!doctype ... length was 9830 ... </script>" } }
|]

