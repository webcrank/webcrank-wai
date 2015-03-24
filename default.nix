{ mkDerivation, base, bytestring, containers, exceptions, mtl
, stdenv, transformers, unix-compat, wai, webcrank
, webcrank-dispatch
}:
mkDerivation {
  pname = "webcrank-wai";
  version = "0.1";
  src = ./.;
  buildDepends = [
    base bytestring containers exceptions mtl transformers unix-compat
    wai webcrank webcrank-dispatch
  ];
  homepage = "https://github.com/webcrank/webcrank-wai";
  description = "Build a WAI Application from Webcrank Resources";
  license = stdenv.lib.licenses.bsd3;
}
