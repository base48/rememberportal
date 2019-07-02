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

        orgName = mkOption {
          type = types.str;
          default = "memberportal";
          description = "Organization name shown in the page header";
        };

        sendmailBin = mkOption {
          type = types.str;
          default = "/run/current-system/sw/bin/sendmail";
          description = "Path to sendmail binary (see also: msmtp)";
        };

        mailFrom = mkOption {
          type = types.str;
          default = "noreply@example.com";
          description = "Email address to use in From: header";
        };

        user = mkOption {
          type = types.str;
          default = projectName;
          description = "User for the daemon.";
        };

        group = mkOption {
          type = types.str;
          default = projectName;
          description = "Group for the daemon.";
        };

        stateDir = mkOption {
          type = types.path;
          default = varLibState;
          description = "State directory of the daemon.";
        };

        port = mkOption {
          type = types.port;
          default = 13337;
          description = "Port number on which the application listens. Note that it always listens on loopback interface because https proxy in front of it is required.";
        };
      };
    };

    config = mkIf cfg.enable {
      environment.systemPackages = [ rememberportalPackage ];
      systemd.services.rememberportal = {
        description = "Member portal";
        environment = {
          RMP_STATIC_DIR = "${rememberportalPackage}/static/";
          RMP_HOST = "127.0.0.1";
          RMP_PORT = "${toString cfg.port}";
          RMP_IP_FROM_HEADER = "true";
          RMP_SENDMAIL_BIN = "${cfg.sendmailBin}";
          RMP_MAIL_FROM = "${cfg.mailFrom}";
          RMP_ORG_NAME = "${cfg.orgName}";
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

      users.users = optionalAttrs (cfg.user == projectName) (singleton {
        isSystemUser = true;
        name = projectName;
        group = cfg.group;
        home = cfg.stateDir;
        createHome = (cfg.stateDir == varLibState);
        description = "User for ${projectName} daemon";
      });

      users.groups = optionalAttrs (cfg.group == projectName) (singleton {
        name = projectName;
      });

    };
  }
