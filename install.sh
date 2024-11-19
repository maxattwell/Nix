#!/usr/bin/env bash

source "uname.sh"

src="$HOME/nix-configs"
if [ "$linux_os" = "nixos" ]
then
    dst="/etc/nixos"
    sudo ln -sfn "$src/flake.nix" "$dst/flake.nix"
    sudo ln -sfn "$src/flake.lock" "$dst/flake.lock"
    sudo ln -sfn "$src/hosts" "$dst/hosts"
    sudo ln -sfn "$src/modules" "$dst/modules"
fi
echo "generated soft links to $dst"
