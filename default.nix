{ stdenv, fetchFromGitHub, ocaml, findlib, batteries, parsercombinator }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-net_codecs";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "ocalme-net-codecs";
    rev = "v${version}";
    sha256 = "0cvy1mjqg967hfr611gwbg3aiblz5bg9k5ppk9xrcg4d3yx08dnz";
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
