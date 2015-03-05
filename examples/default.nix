{ mkDerivation, base, stdenv, webcrank-wai }:
mkDerivation {
  pname = "webcrank-wai-examples";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base webcrank-wai ];
  homepage = "https://github.com/webcrank/webcrank-wai";
  description = "Example WAI Application built with Webcrank";
  license = stdenv.lib.licenses.bsd3;
}
