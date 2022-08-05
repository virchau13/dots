# Discord + OpenAsar + latest Electron
# Credit to https://github.com/jakehamilton/config/blob/2b26bd3a99a896f2f7d7d7d83d387ff00f30df6e/packages/discord/default.nix
# for the Electron version bump, and fortuneteller2k in nixpkgs for OpenAsar

{ pname ? "discord", version ? "0.0.18", src, binaryName ? "Discord", desktopName ? "Discord", autoPatchelfHook
, makeDesktopItem, lib, stdenv, wrapGAppsHook, makeShellWrapper, alsa-lib, at-spi2-atk
, at-spi2-core, atk, cairo, cups, dbus, expat, fontconfig, freetype, gdk-pixbuf
, glib, gtk3, libcxx, libdrm, libnotify, libpulseaudio, libuuid, libX11
, libXScrnSaver, libXcomposite, libXcursor, libXdamage, libXext, libXfixes
, libXi, libXrandr, libXrender, libXtst, libxcb, libxshmfence, mesa, nspr, nss
, pango, systemd, libappindicator-gtk3, libdbusmenu, writeScript, ffmpeg, libatomic_ops
, fetchurl, common-updater-scripts, electron_16, callPackage }:

let

openasar = callPackage ./openasar.nix {};

in

stdenv.mkDerivation rec {
  inherit pname version;
  
  src = fetchurl {
    url =
      "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
    sha256 = "1hl01rf3l6kblx5v7rwnwms30iz8zw6dwlkjsx2f1iipljgkh5q4";
  };

  nativeBuildInputs = [
    alsa-lib
    autoPatchelfHook
    cups
    libdrm
    libuuid
    libXdamage
    libX11
    libXScrnSaver
    libXtst
    libxcb
    libxshmfence
    mesa
    nss
    wrapGAppsHook
    makeShellWrapper
  ];

  dontWrapGApps = true;

  libPath = lib.makeLibraryPath [
    libcxx
    systemd
    libpulseaudio
    libdrm
    mesa
    stdenv.cc.cc
    alsa-lib
    atk
    at-spi2-atk
    at-spi2-core
    cairo
    cups
    dbus
    expat
    fontconfig
    freetype
    gdk-pixbuf
    glib
    gtk3
    libnotify
    libX11
    libXcomposite
    libuuid
    libXcursor
    libXdamage
    libXext
    libXfixes
    libXi
    libXrandr
    libXrender
    libXtst
    nspr
    nss
    libxcb
    pango
    libXScrnSaver
    libappindicator-gtk3
    libdbusmenu
    ffmpeg
    libatomic_ops
  ];

  installPhase = ''
    runHook preInstall

    rm -rf *.so ${binaryName} chrome-sandbox swiftshader
    mkdir -p $out/{bin,opt/${binaryName},share/pixmaps,share/icons/hicolor/256x256/apps}
    mv * $out/opt/${binaryName}

    cp -f ${openasar} $out/opt/${binaryName}/resources/app.asar

    makeWrapper ${electron_16}/bin/electron $out/opt/${binaryName}/${binaryName} \
        "''${gappsWrapperArgs[@]}" \
        --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--enable-features=UseOzonePlatform --ozone-platform=wayland}}" \
        --add-flags $out/opt/${binaryName}/resources/app.asar \
        --run "cd $out/opt/${binaryName}/resources/" \
        --prefix XDG_DATA_DIRS : "${gtk3}/share/gsettings-schemas/${gtk3.name}/" \
        --prefix LD_LIBRARY_PATH : ${libPath}:$out/opt/${binaryName}

    ln -s $out/opt/${binaryName}/${binaryName} $out/bin/
    # Without || true the install would fail on case-insensitive filesystems
    ln -s $out/opt/${binaryName}/${binaryName} $out/bin/${
      lib.strings.toLower binaryName
    } || true

    ln -s $out/opt/${binaryName}/discord.png $out/share/pixmaps/${pname}.png
    ln -s $out/opt/${binaryName}/discord.png $out/share/icons/hicolor/256x256/apps/${pname}.png

    ln -s "${desktopItem}/share/applications" $out/share/

    runHook postInstall
  '';

  desktopItem = makeDesktopItem {
    name = pname;
    exec = binaryName;
    icon = pname;
    inherit desktopName;
    genericName = meta.description;
    categories = [ "Network" "InstantMessaging" ];
    mimeTypes = [ "x-scheme-handler/discord" ];
  };

  passthru.updateScript = writeScript "discord-update-script" ''
    #!/usr/bin/env nix-shell
    #!nix-shell -i bash -p curl gnugrep common-updater-scripts
    set -eou pipefail;
    url=$(curl -sI "https://discordapp.com/api/download/${
      builtins.replaceStrings [ "discord-" "discord" ] [ "" "stable" ] pname
    }?platform=linux&format=tar.gz" | grep -oP 'location: \K\S+')
    version=''${url##https://dl*.discordapp.net/apps/linux/}
    version=''${version%%/*.tar.gz}
    update-source-version ${pname} "$version" --file=./pkgs/applications/networking/instant-messengers/discord/default.nix
  '';

  meta = with lib; {
    description = "All-in-one cross-platform voice and text chat for gamers";
    homepage = "https://discordapp.com/";
    downloadPage = "https://discordapp.com/download";
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    license = licenses.unfree;
    maintainers = with maintainers; [ ldesgoui MP2E devins2518 ];
    platforms = [ "x86_64-linux" ];
  };
}
