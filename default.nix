{ mkDerivation, attoparsec, base, base64-bytestring, byteable
, bytestring, cereal, cipher-aes, cryptohash, deepseq, hex
, QuickCheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "hmacaroons";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    attoparsec base base64-bytestring byteable bytestring cereal
    cipher-aes cryptohash deepseq hex
  ];
  testDepends = [
    attoparsec base base64-bytestring byteable bytestring cereal
    cryptohash hex QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/jtanguy/hmacaroons";
  description = "Haskell implementation of macaroons";
  license = stdenv.lib.licenses.bsd3;
}
