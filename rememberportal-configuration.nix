{
  network.description = "rememberportal";

  rememberportal =
    { config, pkgs, ... }:
    {
      imports = [
        ./rememberportal/module.nix
      ];

      networking.hostName = "rememberportal";

      networking.firewall.allowedTCPPorts = [ 22 80 3000 ];
      environment.systemPackages = [ pkgs.msmtp pkgs.vim ];
      services.rememberportal.enable = true;
    };
}
