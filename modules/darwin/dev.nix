{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    brave

    nodejs_22
    pnpm
    yarn
    supabase-cli
    watchman
    # Java OpenJDK
    zulu17
    eas-cli
    claude-code
    opencode
  ];

  environment.variables = {
    JAVA_HOME = "${pkgs.zulu17}/Library/Java/JavaVirtualMachines/zulu-17.jdk/Contents/Home";
    ANDROID_HOME = "/Users/max/Library/Android/sdk";
  };

  environment.systemPath = [
    "$ANDROID_HOME/emulator"
    "$ANDROID_HOME/platform-tools"
    "$HOME/go/bin"
  ];
}
