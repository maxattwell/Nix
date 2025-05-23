# Nix Configurations
Collection of Nix configurations for Max Attwell.

## NixOS Setup
1. Boot from NixOS installation media

2. Enter a nix shell with git
``` sh
nix-shell -p git
```

3. Clone this repository
``` sh
git clone git@github.com:maxattwell/Nix.git
cd Nix
```

4. Install NixOS with the nixbox configuration
``` sh
sudo nixos-install --flake .#nixbox
```

5. Reboot into your new system
``` sh
sudo reboot
```

6. After first boot, you can update your system with
``` sh
sudo nixos-rebuild switch --flake .#nixbox
```

## Darwin Setup
0. Set username as `max`.

1. Set hostname 

``` sh
sudo scutil—set HostName macbookair 
```

2. Install nix-darwin

3. Download this repo
``` sh
git clone https://github.com/maxattwell/Nix.git
```

4. Setup flake

``` sh
nix run nix-darwin —- switch -—flake .
```

5. Restart

6. Disable system integrity protection 


## Git Setup 
1. Create ssh key
``` sh
# Generate a new ssh key for github
ssh-keygen -t ed25519 -C max.attwell@hotmail.com

# Start ssh agent
eval "$(ssh-agent -s)"

# Add key to ssh agent
ssh-add ~/.ssh/id_ed25519
```

3. Add `~/.ssh/config`

```yaml
Host github.com
  User git
  Hostname github.com
  PreferredAuthentications publickey
  IdentityFile /home/max/.ssh/id_ed25519
```
