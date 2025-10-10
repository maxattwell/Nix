{ config, lib, pkgs, ... }:

{
  programs.git = {
      enable = true;
      userName  = "Max Attwell";
      userEmail = "max.attwell@hotmail.com";
      extraConfig = {
        init.defaultBranch = "main";
        github.user = "maxattwell";
      };
      delta = {
        enable = true;
        options = {
          line-numbers = true;
        };
      };
    };
}
