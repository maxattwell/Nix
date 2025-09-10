{ config, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/doom
    ../../modules/home-manager/sketchybar
  ];

  home = {
    username = "max";
    homeDirectory = "/Users/max";
    stateVersion = "24.11";


    sessionVariables = {
      JAVA_HOME = "${pkgs.zulu17}/Library/Java/JavaVirtualMachines/zulu-17.jdk/Contents/Home";
      ANDROID_HOME = "/Users/max/Library/Android/sdk";
    };

    sessionPath = [
      "$ANDROID_HOME/emulator"
      "$ANDROID_HOME/platform-tools"
      "$HOME/go/bin"
    ];
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
