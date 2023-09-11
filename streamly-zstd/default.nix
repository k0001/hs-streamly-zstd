{ mkDerivation, base, bytestring, exceptions, lib, streamly-core
, zstd
}:
mkDerivation {
  pname = "streamly-zstd";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring exceptions streamly-core zstd
  ];
  homepage = "https://github.com/k0001/hs-streamly-zstd";
  description = "Streaming ZSTD compression and decompression through Streamly";
  license = "unknown";
}
