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
      github.user = "maxattwell";
    };

    includes = [
      {
        condition = "gitdir:~/code/insurgence/";
        contents = {
          user = {
            email = "max.attwell@insurgence.ai";
          };
        };
      }
    ];
  };

  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      line-numbers = true;
    };
  };
}
