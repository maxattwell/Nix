{ config, lib, pkgs, ... }:

{
  programs.git = {
      enable = true;
      settings = {
        user = {
          name = "Max Attwell";
          email = "max.attwell@hotmail.com";
        };
        init.defaultBranch = "main";
      };
    };

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      line-numbers = true;
    };
  };
}
