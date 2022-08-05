{ lib, stdenv, fetchzip, libusb, glibc, libGL, xorg, qtx11extras, wrapQtAppsHook, autoPatchelfHook, ... }:

stdenv.mkDerivation {
    pname = "xp-pen-deco-01-v2-driver";
    version = "3.2.3.220323-1";
    src = fetchzip {
        url = "https://www.xp-pen.com/download/file/id/1936/pid/440/ext/gz.html#.tar.gz";
        sha256 = "sha256-n/yutkRsjcIRRhB4q1yqEmaa03/1SO8RigJi/ZkfLbk=";
    };

    nativeBuildInputs = [
        wrapQtAppsHook
        autoPatchelfHook
    ];

    buildInputs = [
        libusb
        xorg.libX11
        xorg.libXtst
        xorg.libXi
        xorg.libXrandr
        xorg.libXinerama
        glibc
        libGL
        stdenv.cc.cc.lib
        qtx11extras
    ];

    installPhase = let dataDir = "var/lib/xppend1v2"; in ''
        mkdir -p $out/{opt,bin}
        cp -r App/usr/lib/pentablet/{pentablet,resource.rcc,conf} $out/opt
        chmod +x $out/opt/pentablet
        cp -r App/lib $out/lib
        sed -i 's#usr/lib/pentablet#${dataDir}#g' $out/opt/pentablet
        makeWrapper $out/opt/pentablet $out/bin/xp-pen-deco-01-v2-driver \
            --run 'if [ "$EUID" -ne 0 ]; then echo "Please run as root"; exit 1; fi' \
            --run 'if [ ! -d /${dataDir} ]; then mkdir -p /${dataDir}; cp -r '$out'/opt/conf /${dataDir}; chmod u+w -R /${dataDir}; fi'
    '';

    preFixup = ''
        wrapQtApp $out/opt/pentablet
    '';

    meta = with lib; {
        homepage = "https://www.xp-pen.com/product/461.html";
        description = "Drivers for the XP-PEN Deco 01 v2 drawing tablet";
        platforms = [ "x86_64-linux" ];
        sourceProvenance = with sourceTypes; [ binaryNativeCode ];
        # TODO maintainers
        license = licenses.unfree;
    };
}
