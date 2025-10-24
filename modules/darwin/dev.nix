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
    # claude-code
    # opencode

    azure-cli
    uv
    python312

    # Vue language server for Emacs LSP (requires TypeScript)
    nodePackages."@vue/language-server"
    nodePackages.typescript
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
