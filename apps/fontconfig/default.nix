{ inputs, pkgs, lib, ... }: {
    fonts = {
        fonts = with pkgs; [
            noto-fonts
            noto-fonts-cjk
            noto-fonts-emoji
            liberation_ttf
            fira-code
            fira-code-symbols
            nerdfonts
            open-sans
            (input-fonts.overrideAttrs(old: {
                pname = "${old.pname}-nerd-font";
                buildInputs = [
                    fontforge
                    (python2.withPackages (pypkgs: [pypkgs.configparser]))
                ];
                postInstall = ''
                    export LANG=en_US.UTF-8
                    find $out/ -type f -name '*.ttf' -exec fontforge ${pkgs.nerd-font-patcher}/bin/font-patcher {} \; 
                '';
            }))
        ];
    } // (if pkgs.stdenv.isLinux then {
        enableDefaultFonts = true;
        fontconfig.localConf = builtins.readFile ../../apps/fontconfig/fonts.xml;
    } else if pkgs.stdenv.isDarwin then {
        enableFontDir = true;
    } else {});
    nixpkgs.config.input-fonts.acceptLicense = true;
}
