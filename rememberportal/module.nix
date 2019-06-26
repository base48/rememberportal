{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.rememberportal;
  rememberportalPackage = import ./. {}; # FIXME inherit pkgs?
  projectName = "rememberportal";
  varLibState = "/var/lib/${projectName}";
in
  {
    options = {
      services.rememberportal = {
        enable = mkEnableOption "rememberportal";

        user = mkOption {
          type = types.str;
          default = "rememberportal";
          description = "User for the daemon.";
        };

        group = mkOption {
          type = types.str;
          default = "rememberportal";
          description = "Group for the daemon.";
        };

        stateDir = mkOption {
          type = types.path;
          default = varLibState;
          description = "State directory of the daemon.";
        };
      };
    };

    config = mkIf cfg.enable {
      environment.systemPackages = [ rememberportalPackage ];
      systemd.services.rememberportal = {
        description = "Member portal";
        environment = {
          YESOD_STATIC_DIR = "${rememberportalPackage}/static/";
          YESOD_HOST = "localhost";
          YESOD_PORT = "3000";
          YESOD_IP_FROM_HEADER = "true";
        };
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          ExecStart = "${rememberportalPackage}/bin/rememberportal";
          WorkingDirectory = "${cfg.stateDir}";
          User = cfg.user;
          Group = cfg.group;
          #PermissionsStartOnly = true; # needed for startPre etc.
        };
      };

      users.users = optionalAttrs (cfg.user == "rememberportal") (singleton {
        isSystemUser = true;
        name = projectName;
        group = cfg.group;
        home = cfg.stateDir;
        createHome = (cfg.stateDir == varLibState);
      });

      users.groups = optionalAttrs (cfg.group == "rememberportal") (singleton {
        name = projectName;
      });

    };
  }
