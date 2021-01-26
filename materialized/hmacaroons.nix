{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "hmacaroons"; version = "0.5.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "julien.tanguy@jhome.fr";
      author = "Julien Tanguy";
      homepage = "https://github.com/jtanguy/hmacaroons";
      url = "";
      synopsis = "Haskell implementation of macaroons";
      description = "Hmacaroons is a pure haskell implementation of macaroons. It aims to\nprovide compatibility at a serialized level with the\n<https://github.com/rescrv/libmacaroons reference implementation> and\nthe <https://github.com/ecordell/pymacaroons python implementation>\n\n__WARNING: This library has not been audited by security experts.__\n__There is no error handling at the moment, everything is silently accepted__\n\nIt is developed in the purpose of exploration purposes, and would need\nmuch more attention if it were to be used in production.\n\n= References\n#references#\n\n== Papers and articles\n#papers-and-articles#\n\n-   <http://research.google.com/pubs/pub41892.html Google paper on macaroons>\n-   <https://air.mozilla.org/macaroons-cookies-with-contextual-caveats-for-decentralized-authorization-in-the-cloud/ Macaroons at Mozilla>\n-   <http://hackingdistributed.com/2014/11/23/macaroons-in-hyperdex/ Time for better security in NoSQL>\n\n== Implementations\n#implementations#\n\n-   <https://github.com/rescrv/libmacaroons C>\n-   <https://github.com/nitram509/jmacaroons Java>\n-   <https://github.com/nitram509/macaroons.js Node.js>\n-   <https://github.com/ecordell/pymacaroons Python>\n-   <https://github.com/cryptosphere/rust-macaroons.git Rust>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "CONTRIBUTING.md" "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
          (hsPkgs."either" or (errorHandler.buildDepError "either"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hex" or (errorHandler.buildDepError "hex"))
          ];
        buildable = true;
        modules = [
          "Crypto/Macaroon/Internal"
          "Crypto/Macaroon/Verifier/Internal"
          "Crypto/Macaroon"
          "Crypto/Macaroon/Serializer/Base64"
          "Crypto/Macaroon/Verifier"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            (hsPkgs."either" or (errorHandler.buildDepError "either"))
            (hsPkgs."hex" or (errorHandler.buildDepError "hex"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          hsSourceDirs = [ "src" "test" ];
          mainPath = [ "main.hs" ];
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."cryptohash" or (errorHandler.buildDepError "cryptohash"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."either" or (errorHandler.buildDepError "either"))
            (hsPkgs."hex" or (errorHandler.buildDepError "hex"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          hsSourceDirs = [ "src" "bench" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./.;
    }