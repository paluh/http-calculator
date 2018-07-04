{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-compat, attoparsec, base
      , base-compat, blaze-html, blaze-markup, bytestring, containers
      , cookie, directory, ghcid, http-api-data, http-client, http-media
      , http-types, js-jquery, lucid, mtl, mtl-compat, random, servant
      , servant-client, servant-docs, servant-js, servant-server, stdenv
      , string-conversions, text, time, transformers, wai, warp
      }:
      mkDerivation {
        buildDepends = [ ghcid ];
        pname = "http-calculator";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson aeson-compat attoparsec base base-compat blaze-html
          blaze-markup bytestring containers cookie directory http-api-data
          http-client http-media http-types js-jquery lucid mtl mtl-compat
          random servant servant-client servant-docs servant-js
          servant-server string-conversions text time transformers wai warp
        ];
        homepage = "https://github.com/paluh/http-calculator";
        description = "Servant exercise";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
