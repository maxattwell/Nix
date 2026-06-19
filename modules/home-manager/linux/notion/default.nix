{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    appimage-run
  ];

  xdg.desktopEntries.notion-electron = {
    name = "Notion";
    exec = "appimage-run ${config.home.homeDirectory}/Applications/Notion_Electron-2.2.2-x86_64.AppImage";
    icon = "${config.home.homeDirectory}/Applications/notion-electron.png";
    terminal = false;
    categories = [ "Office" ];
  };
}
