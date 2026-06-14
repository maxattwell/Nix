{ pkgs, inputs, ... }:

{
  fonts.packages = with pkgs; [
    nerd-fonts.overpass
  ];

  environment.systemPackages = [
    inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.rose-pine-hyprcursor.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];
}
