{ mkDerivation, base, bytestring, containers, exceptions
, http-types, lens, mtl, network, reroute, stdenv, text
, transformers, unix-compat, unordered-containers, vault, wai
, wai-lens, webcrank, webcrank-dispatch
}:
mkDerivation {
  pname = "webcrank-wai";
  version = "0.2";
  src = ./.;
  buildDepends = [
    base bytestring containers exceptions http-types lens mtl network
    reroute text transformers unix-compat unordered-containers vault
    wai wai-lens webcrank webcrank-dispatch
  ];
  homepage = "https://github.com/webcrank/webcrank-wai";
  description = "Build a WAI Application from Webcrank Resources";
  license = stdenv.lib.licenses.bsd3;
}
