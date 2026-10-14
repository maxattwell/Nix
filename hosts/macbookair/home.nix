{ config, pkgs, ... }:

{
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/doom
  ];

  home = {
    stateVersion = "24.11";

    sessionVariables = {
      JAVA_HOME = "${pkgs.zulu17}/Library/Java/JavaVirtualMachines/zulu-17.jdk/Contents/Home";
      ANDROID_HOME = "/Users/max/Library/Android/sdk";
      NPM_CONFIG_PREFIX = "~/.npm-global";
      PNPM_HOME = "~/.local/share/pnpm";
      BUN_INSTALL = "~/.bun";
      XDG_CONFIG_HOME ="~/.config";
    };

    sessionPath = [
      "$ANDROID_HOME/emulator"
      "$ANDROID_HOME/platform-tools"
      "$HOME/go/bin"
      "$PNPM_HOME"
    ];
  };

   # Create npm global directory
   home.activation.createNpmGlobalDir = ''
     mkdir -p ~/.npm-global
   '';

   # Create pnpm global directory
   home.activation.createPnpmHome = ''
     mkdir -p ~/.local/share/pnpm
   '';

   # Create bun global directory
   home.activation.createBunHome = ''
     mkdir -p ~/.bun/bin
   '';
}
