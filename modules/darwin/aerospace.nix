{ config, pkgs, ... }:

{
  services.aerospace = {
    enable = true;
    settings = {
      default-root-container-layout = "tiles";

      gaps = {
        outer = {
          top = 35;
          left = 2;
          right = 2;
          bottom = 2;
        };
      };

      # Mouse follows focus - move mouse to center of focused window
      on-focus-changed = ["move-mouse window-lazy-center"];

      # Notify Sketchybar about workspace change
      exec-on-workspace-change = [
        "${pkgs.bash}/bin/bash"
        "-c"
        "${pkgs.sketchybar}/bin/sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$AEROSPACE_FOCUSED_WORKSPACE PREV_WORKSPACE=$AEROSPACE_PREV_WORKSPACE"
      ];

      mode.main.binding = {
        cmd-h = "focus left";
        cmd-j = "focus down";
        cmd-k = "focus up";
        cmd-l = "focus right";

        cmd-shift-h = "move left";
        cmd-shift-j = "move down";
        cmd-shift-k = "move up";
        cmd-shift-l = "move right";

        cmd-1 = "workspace 1";
        cmd-2 = "workspace 2";
        cmd-3 = "workspace 3";
        cmd-4 = "workspace 4";
        cmd-5 = "workspace 5";
        cmd-6 = "workspace 6";
        cmd-7 = "workspace 7";
        cmd-8 = "workspace 8";
        cmd-9 = "workspace 9";
        cmd-0 = "workspace 10";

        cmd-shift-1 = "move-node-to-workspace --focus-follows-window 1";
        cmd-shift-2 = "move-node-to-workspace --focus-follows-window 2";
        cmd-shift-3 = "move-node-to-workspace --focus-follows-window 3";
        cmd-shift-4 = "move-node-to-workspace --focus-follows-window 4";
        cmd-shift-5 = "move-node-to-workspace --focus-follows-window 5";
        cmd-shift-6 = "move-node-to-workspace --focus-follows-window 6";
        cmd-shift-7 = "move-node-to-workspace --focus-follows-window 7";
        cmd-shift-8 = "move-node-to-workspace --focus-follows-window 8";
        cmd-shift-9 = "move-node-to-workspace --focus-follows-window 9";
        cmd-shift-0 = "move-node-to-workspace --focus-follows-window 10";

        # Shortcuts
        cmd-enter = "exec-and-forget osascript -e 'tell application \"iTerm2\" to create window with default profile'";
        cmd-e = "exec-and-forget emacsclient -cn";
        cmd-shift-enter = "exec-and-forget osascript -e 'tell application \"Google Chrome\" to make new window'";
        cmd-q = "close";
        cmd-p = "workspace-back-and-forth";
        cmd-o = "layout tiles accordion horizontal vertical";
        cmd-shift-s = "exec-and-forget screencapture -i -c";
      };
    };
  };


  # Disable screenshot shortcuts
  system.defaults.CustomUserPreferences = {
    "com.apple.symbolichotkeys" = {
      AppleSymbolicHotKeys = {
        # Disable cmd+shift+3 (save picture of screen as a file)
        "28" = {
          enabled = false;
        };
        # Disable cmd+shift+4 (save picture of selected area as a file)
        "29" = {
          enabled = false;
        };
        # Disable cmd+shift+5 (screenshot and recording options)
        "184" = {
          enabled = false;
        };
      };
    };
  };
}
