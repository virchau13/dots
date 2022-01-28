{ pkgs, ... }:
let pname = "xp-pen-deco-01-v2-driver";
    version = "3.2.1.211019-1";
in with pkgs; with pkgs.libsForQt5;
let 
    package-unwrapped = stdenv.mkDerivation {
        pname = pname + "-unwrapped";
        inherit version;
        src = fetchTarball {
            url = "https://www.xp-pen.com/download/file/id/1936/pid/440/ext/gz.html";
            sha256 = "sha256:1vr5ibfnv12h6m8s9am6g557n4a6ca110g4zynhbdm410g1qdrig";
        };
        installPhase = ''
            mkdir -p $out/
            cp -r App/{etc,lib} $out/
            cp -r App/usr/{lib,share} $out/
            substituteInPlace $out/etc/xdg/autostart/xppentablet.desktop \
                --replace "/usr/" "$out/"
            mkdir -p $out/bin
            cat >$out/bin/${pname} <<EOF
#!${bash}/bin/bash
LD_LIBRARY_PATH="$out/lib/pentablet/lib:\$LD_LIBRARY_PATH" "/usr/lib/pentablet/pentablet"
EOF
            chmod +x $out/bin/${pname}
        ''; 
    };
    fhsenv = buildFHSUserEnv {
        name = "xp-pen-fhs-env";
        targetPkgs = pkgs: with pkgs; [
            package-unwrapped
            libGL
            gcc
            libusb1
            zlib
            glib
            glibc
            freetype
            fontconfig
            dbus_daemon
            xorg.setxkbmap
            # Technically included with the package,
            # but required for XKB.
            qtbase
            qtmultimedia
        ] ++ (with pkgs.xlibs; [
            libX11
            libXtst
            libXi
            libXrandr
            libXinerama
            libXext
            libxcb
            libXrender
            libSM
            libICE
        ]);
        extraBuildCommands = ''
            rm -r usr/lib/pentablet/conf
            ln -s /home/hexular/.config/xp-pen-wrapper usr/lib/pentablet/conf
        '';
    };
    package = writeShellScriptBin pname ''
        set -e
        if [ ! -d ~/.config/xp-pen-wrapper ]; then
            cp -r ${package-unwrapped}/lib/pentablet/conf ~/.config/xp-pen-wrapper
        fi
        exec ${fhsenv}/bin/xp-pen-fhs-env ${package-unwrapped}/bin/${pname}
    '';
in {
    environment.systemPackages = [ package ];
    services.udev.packages = [ package ];
}
