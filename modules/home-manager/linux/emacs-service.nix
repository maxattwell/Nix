{ pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    startWithUserSession = "graphical";
    defaultEditor = true;
    client.enable = true;
    socketActivation.enable = true;
  };
}
