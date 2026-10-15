{ pkgs, ... }:

{
  # Sunshine game-stream host (for Moonlight clients).
  #
  # Runtime settings are deliberately kept out of Nix, in
  # ~/.config/sunshine/:
  #   sunshine.conf  -> capture = wlr, encoder = nvenc, output_name = virtual
  #   apps.json      -> repositions the "virtual" headless panel to (0,0)
  #                     while streaming (needed for absolute mouse on Linux)
  services.sunshine = {
    enable = true;
    autoStart = true;
    openFirewall = true;
    capSysAdmin = true; # DRM/KMS capture on Wayland

    # Nixpkgs builds Sunshine without CUDA, which compiles out NVENC. The
    # GTX 1070 (Pascal/sm_61) needs a CUDA 12.x toolchain; CUDA 13 dropped it.
    package = pkgs.sunshine.override {
      cudaSupport = true;
      cudaPackages = pkgs.cudaPackages;
    };
  };

  # Sunshine injects keyboard/mouse/gamepad via /dev/uinput.
  users.users.max.extraGroups = [ "uinput" ];
}
