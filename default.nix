{ stdenv, fetchFromGitHub, ocaml, findlib, batteries, parsercombinator }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-net_codecs";
  version = "1.1";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "ocalme-net-codecs";
    rev = "v${version}";
    sha256 = "0h84snb8km4dy432qmhf1h4dcqdyq1bncjw7p1lpys1b1pzgrvrx";
  };

  buildInputs = [ ocaml findlib batteries parsercombinator ];

  createFindlibDestdir = true;

  meta = with stdenv.lib; {
    homepage = https://github.com/rixed/ocalme-net-codecs;
    description = "Primitive decoder for some protocols";
    platforms = ocaml.meta.platforms or [];
    maintainers = [ maintainers.rixed ];
  };
}
