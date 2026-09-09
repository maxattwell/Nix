{ config, lib, pkgs, ... }:

let
  home = config.home.homeDirectory;
  emacsPath = lib.concatStringsSep ":" [
    "${home}/.config/emacs/bin"
    "${home}/.npm-global/bin"
    "${home}/.local/bin"
    "${home}/.local/share/pnpm"
    "${home}/.bun/bin"
    "${home}/.pi/agent/bin"
    "${home}/.nix-profile/bin"
    "/etc/profiles/per-user/${config.home.username}/bin"
    "/run/wrappers/bin"
    "/run/current-system/sw/bin"
  ];
in
{
  services.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    # Socket activation starts Emacs on first emacsclient use.
    startWithUserSession = false;
    defaultEditor = true;
    client.enable = true;
    socketActivation.enable = true;
  };

  # The Emacs daemon is launched by systemd, not by an interactive shell. Give
  # it the tool PATH/env that Pilish subprocesses need.
  systemd.user.services.emacs.Service = {
    ExecStart = lib.mkForce "${pkgs.emacs-pgtk}/bin/emacs --fg-daemon=%t/emacs/server";
    Environment = [
      "PATH=${emacsPath}"
      "NOTION_KEYRING=0"
    ];
  };
}
