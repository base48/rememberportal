{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.rememberportal;
  rememberportalPackage = import ./. {}; # FIXME inherit pkgs?
  projectName = "rememberportal";
  stateDir = "/var/lib/${projectName}";
  user = projectName;
  group = projectName;

  env = {
    RMP_STATIC_DIR = "${rememberportalPackage}/static/";
    RMP_HOST = "127.0.0.1";
    RMP_PORT = "${toString cfg.port}";
    RMP_IP_FROM_HEADER = "true";
    RMP_SENDMAIL_BIN = cfg.sendmailBin;
    RMP_MAIL_FROM = cfg.mailFrom;
    RMP_ORG_NAME = cfg.orgName;
    RMP_CURRENCY = cfg.currency;
  };
  srv = name: {
    ExecStart = "${rememberportalPackage}/bin/${name}";
    WorkingDirectory = stateDir;
    User = user;
    Group = group;
    #PermissionsStartOnly = true; # needed for startPre etc.
  };
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

        currency = mkOption {
          type = types.str;
          default = "BTC";
          description = "Membership fees currency";
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
      systemd.services = {
        rememberportal = {
          description = "Member portal";
          environment = env;
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig = srv "rememberportal";
        };

        rememberportal-create-fees = {
          description = "Member portal fees task";
          environment = env;
          after = [ "rememberportal.service" ];
          requires = [ "rememberportal.service" ];
          serviceConfig = srv "rememberportal-create-fees";
        };
      };

      systemd.timers = {
        rememberportal-create-fees = {
          description = "Periodically insert new membership fees";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "monthly";
            Persistent = true;
          };
        };
      };

      users.users = (singleton {
        isSystemUser = true;
        name = projectName;
        group = group;
        home = stateDir;
        createHome = true;
        description = "User for ${projectName} daemon";
      });

      users.groups = (singleton {
        name = projectName;
      });

    };
  }
