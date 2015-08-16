{ mkDerivation, attoparsec, base, base64-bytestring, byteable
, bytestring, cereal, cryptohash, deepseq, either, hex, QuickCheck
, stdenv, tasty, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "hmacaroons";
  version = "0.2.0.0";
  src = ./.;
  buildDepends = [
    attoparsec base base64-bytestring byteable bytestring cereal
    cryptohash deepseq either hex transformers
  ];
  testDepends = [
    attoparsec base base64-bytestring byteable bytestring cereal
    cryptohash either hex QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/jtanguy/hmacaroons";
  description = "Haskell implementation of macaroons";
  license = stdenv.lib.licenses.bsd3;
}
