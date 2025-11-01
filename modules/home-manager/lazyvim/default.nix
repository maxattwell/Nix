{ config, lib, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      lazygit
    ];

    sessionVariables = {
      NVIM_DIR = "$HOME/.config/nvim";
    };
  };

  # Symlink config files for easy editing without rebuilds
  xdg.configFile = {
    # Main init file
    "nvim/init.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/init.lua";

    # Config directory
    "nvim/lua/config/autocmds.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/lua/config/autocmds.lua";
    "nvim/lua/config/keymaps.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/lua/config/keymaps.lua";
    "nvim/lua/config/options.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/lua/config/options.lua";
    "nvim/lua/config/lazy.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/lua/config/lazy.lua";

    # Plugins directory
    "nvim/lua/plugins/colorscheme.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/lua/plugins/colorscheme.lua";
    "nvim/lua/plugins/claude-code.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/lua/plugins/claude-code.lua";
    "nvim/lua/plugins/example.lua".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/lua/plugins/example.lua";

    # Config files
    "nvim/lazyvim.json".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/lazyvim.json";
    "nvim/.neoconf.json".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/.neoconf.json";
    "nvim/stylua.toml".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Nix/modules/home-manager/lazyvim/stylua.toml";
  };
}
