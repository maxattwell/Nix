{ lib, pkgs, ... }:

let
  browser =
    if pkgs.stdenv.hostPlatform.isAarch64
    then pkgs.chromium
    else pkgs.google-chrome;

  default-browser = pkgs.writeShellScriptBin "default-browser" ''
    if command -v google-chrome-stable >/dev/null 2>&1; then
      exec google-chrome-stable "$@"
    elif command -v google-chrome >/dev/null 2>&1; then
      exec google-chrome "$@"
    elif command -v chromium >/dev/null 2>&1; then
      exec chromium "$@"
    else
      echo "No supported browser found" >&2
      exit 1
    fi
  '';
in
{
  environment.systemPackages = [
    browser
    default-browser
  ];
}
