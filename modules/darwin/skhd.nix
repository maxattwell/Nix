# modules/darwin/skhd.nix
{ config, pkgs, ... }:

{
  services.skhd = {
    enable = true;
    skhdConfig = ''
      cmd - return : osascript -e 'tell application "iTerm" to create window with default profile'
      cmd + shift - return : brave
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

      # focus numbered spaces
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
      cmd + shift - 6: yabai -m window --space 6; yabai -m space --focus 6
      cmd + shift - 7: yabai -m window --space 7; yabai -m space --focus 7
      cmd + shift - 8: yabai -m window --space 8; yabai -m space --focus 8
      cmd + shift - 9: yabai -m window --space 9; yabai -m space --focus 9
      cmd + shift - 0: yabai -m window --space 10; yabai -m space --focus 10
    '';
  };
}
