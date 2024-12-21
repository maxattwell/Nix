{ config,  pkgs, ... }:

{

  imports = import (../home-manager/home.nix);

  nix.settings.experimental-features = "nix-command flakes";

  security.pam.enableSudoTouchIdAuth = true;


  environment.systemPackages = with pkgs; [
    emacs-29
    nerdfonts
  ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs-29;
  };

  services.yabai = {
    enable = true;
    enableScriptingAddition = false;
    config = {
        focus_follows_mouse = "autoraise";
        mouse_follows_focus = "on";
        top_padding         = 2;
        bottom_padding      = 30;
        left_padding        = 2;
        right_padding       = 2;
        window_gap          = 2;
    };

    extraConfig = ''
      yabai -m signal --add event=dock_did_retart action="sudo yabai --load-sa"
      sudo yabai --load-sa
      yabai -m space --layout           bsp
      yabai -m config mouse_modifier    cmd
      yabai -m config mouse_action1     resize

      yabai -m rule --add app="^Ajustes del Sistema$" manage=off

      # Force management of all workspaces
      yabai -m config --space 2 layout  bsp
      yabai -m config --space 3 layout  bsp
      yabai -m config --space 4 layout  bsp
      yabai -m config --space 5 layout  bsp
      yabai -m config --space 6 layout  bsp
      yabai -m config --space 7 layout  bsp
      yabai -m config --space 8 layout  bsp
      yabai -m config --space 9 layout  bsp
      yabai -m config --space 10 layout  bsp
    '';

  };

  services.skhd = {
    enable = true;
    skhdConfig = ''
      # Shortcuts
      # cmd - return : open -a iTerm "/Users/max"
      #cmd - return : osascript -e 'tell application "iTerm" to create window with default profile'
      cmd - return : open -na kitty
      # cmd + shift - return : open -a "Google Chrome.app"
      cmd + shift - return : osascript -e 'tell application "Google Chrome" to make new window'
      cmd - e : emacsclient -cn
      cmd - q : osascript -e 'tell application (path to frontmost application as text) to close front window'

      # vim window focus movements
      cmd - h : yabai -m window --focus west
      cmd - l : yabai -m window --focus east
      cmd - j : yabai -m window --focus south
      cmd - k : yabai -m window --focus north

      # vim move windows
      cmd + shift - h : yabai -m window --swap west
      cmd + shift - l : yabai -m window --swap east
      cmd + shift - j : yabai -m window --swap south
      cmd + shift - k : yabai -m window --swap north

      # foucs numbered spaces
      cmd - 1: yabai -m space --focus 1
      cmd - 2: yabai -m space --focus 2
      cmd - 3: yabai -m space --focus 3
      cmd - 4: yabai -m space --focus 4
      cmd - 5: yabai -m space --focus 5
      cmd - 6: yabai -m space --focus 6
      cmd - 7: yabai -m space --focus 7
      cmd - 8: yabai -m space --focus 8
      cmd - 9: yabai -m space --focus 9
      cmd - 0: yabai -m space --focus 10

      # focus prev space
      cmd - p: yabai -m space --focus recent

      # move window to numbered spaces
      cmd + shift - 1: yabai -m window --space 1; yabai -m space --focus 1
      cmd + shift - 2: yabai -m window --space 2; yabai -m space --focus 2
      cmd + shift - 3: yabai -m window --space 3; yabai -m space --focus 3
      # cmd + shift - 4: yabai -m window --space 4; yabai -m space --focus 4
      # cmd + shift - 5: yabai -m window --space 5; yabai -m space --focus 5
      cmd + shift - 6: yabai -m window --space 6; yabai -m space --focus 6
      cmd + shift - 7: yabai -m window --space 7; yabai -m space --focus 7
      cmd + shift - 8: yabai -m window --space 8; yabai -m space --focus 8
      cmd + shift - 9: yabai -m window --space 9; yabai -m space --focus 9
      cmd + shift - 0: yabai -m window --space 10; yabai -m space --focus 10
      '';
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;


  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;
}
