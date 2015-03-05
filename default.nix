{ mkDerivation, base, stdenv, wai, webcrank, webcrank-dispatch }:
mkDerivation {
  pname = "webcrank-wai";
  version = "0.0.1";
  src = ./.;
  buildDepends = [ base wai webcrank webcrank-dispatch ];
  homepage = "https://github.com/webcrank/webcrank-wai";
  description = "Build a WAI Application from Webcrank Resources";
  license = stdenv.lib.licenses.bsd3;
}
