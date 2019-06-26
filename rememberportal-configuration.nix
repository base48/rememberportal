{
  network.description = "rememberportal";

  rememberportal =
    { config, pkgs, ... }: let
      rememberportal = import ./rememberportal/default.nix { inherit pkgs; };
    in
    { networking.hostName = "rememberportal";

      networking.firewall.allowedTCPPorts = [ 22 80 3000 ];
      environment.systemPackages = [ rememberportal pkgs.msmtp pkgs.vim ];

      # todo config msmtp
      # todo ssl proxy (letsencrypt?)
      systemd.services.rememberportal =
        { description = "Member portal";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig =
            { ExecStart = "${rememberportal}/bin/rememberportal";
              WorkingDirectory = "/etc/rememberportal";
            };
        };
    };
}
