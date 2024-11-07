{ inputs, lib, config, homeDir, configDir, pkgs, ... }:
let 
    inherit (config.lib.file) mkOutOfStoreSymlink;
in
{
  imports = [
    ../common/home.nix
    ../../apps/eww
  ];

    
  home.packages = with pkgs; [
    prismlauncher
      # ((callPackage "${inputs.nixpkgs}/pkgs/development/compilers/zulu/common.nix" {
      #   dists = {
      #     x86_64-linux = {
      #       zuluVersion = "8.8.0.3";
      #       jdkVersion = "8.0.51";
      #       hash = "";
      #     };
      #   };
      # }).overrideAttrs(old: { 
      #   src = fetchurl {
      #     url = "https://cdn.azul.com/zulu/bin/zulu1.8.0_51-8.8.0.3-x86lx64.zip";
      #     hash = "sha256-gO9rKXAGclnhNLU8OB33Ai+w25kfVUUi5tVasZamwwo=";
      #     curlOpts = "-H Referer:https://www.azul.com/downloads/zulu/";
      #   };
      #   buildInputs = old.buildInputs ++ [ libpng12 ];
      #   preFixup =  builtins.replaceStrings ["LD_LIBRARY_PATH : "] ["LD_LIBRARY_PATH : ${pkgs.freetype}/lib:"] old.preFixup;
      # }))
  ];
}
