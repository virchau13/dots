{ inputs, pkgs, lib, ... }: let
    makeFont = name: file: pkgs.stdenvNoCC.mkDerivation {
        inherit name;
        dontConfigure = true;
        src = file;
        installPhase = ''
            mkdir -p $out/share/fonts
            cp -R $src $out/share/fonts/opentype/
        '';
    };
in {
    fonts = {
        packages = with pkgs; [
            noto-fonts
            noto-fonts-cjk-sans
            noto-fonts-emoji
            liberation_ttf
            fira-code
            fira-code-symbols
            nerdfonts
            google-fonts
            open-sans
            inter
            corefonts
            vistafonts
            # (input-fonts.overrideAttrs(old: {
            #     pname = "input-fonts-nerd-font";
            #     version = input-fonts.version;
            #     buildInputs = [
            #         fontforge
            #         (python3.withPackages (pypkgs: [pypkgs.configparser]))
            #     ];
            #     postInstall = ''
            #         export LANG=en_US.UTF-8
            #         find $out/ -type f -name '*.ttf' -exec fontforge ${pkgs.nerd-font-patcher}/bin/font-patcher {} \; 
            #     '';
            # }))
        ];
    } // (if pkgs.stdenv.isLinux then {
        enableDefaultPackages = true;
        fontconfig.localConf = builtins.readFile ../../apps/fontconfig/fonts.xml;
    } else if pkgs.stdenv.isDarwin then {
        enableFontDir = true;
    } else {});
    nixpkgs.config.input-fonts.acceptLicense = true;
}
