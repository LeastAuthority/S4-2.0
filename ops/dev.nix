{
  devnode =
  { config, pkgs, ... }:
  { environment.systemPackages = [
      pkgs.git
      pkgs.emacs25-nox
    ];
    system.autoUpgrade.enable = true;
    system.autoUpgrade.channel = "https://nixos.org/channels/nixpkgs-unstable";
    system.autoUpgrade.dates = "04:45";
  };
}
