{ inputs, pkgs, ... }: {
    fonts = {
        enableDefaultFonts = true;
        fonts = with pkgs; [
            noto-fonts
            noto-fonts-cjk
            noto-fonts-emoji
            liberation_ttf
            fira-code
            fira-code-symbols
            nerdfonts
            (input-fonts.overrideAttrs(old: {
                pname = "${old.pname}-nerd-font";
                buildInputs = [
                    fontforge
                    (python2.withPackages (pypkgs: [pypkgs.configparser]))
                ];
                postInstall = ''
                    export LANG=en_US.UTF-8
                    find $out/ -type f -name '*.ttf' -exec fontforge ${inputs.nerd-font-patcher.outPath}/font-patcher {} \; 
                '';
            }))
        ];
        fontconfig.localConf = builtins.readFile ../../apps/fontconfig/fonts.xml;
    };
    nixpkgs.config.input-fonts.acceptLicense = true;
}
