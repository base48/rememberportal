{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.rememberportal;
  rememberportalPackage = import ./. { inherit pkgs; };
  projectName = "rememberportal";
  stateDir = "/var/lib/${projectName}";
  user = projectName;
  group = projectName;

  env = {
    RMP_STATIC_DIR = "${rememberportalPackage}/static/";
    RMP_HOST = "127.0.0.1";
    RMP_PORT = toString cfg.port;
    RMP_IP_FROM_HEADER = "true";
    RMP_SENDMAIL_BIN = cfg.mail.sendmail;
    RMP_MAIL_FROM = cfg.mail.from;
    RMP_ORG_NAME = cfg.orgName;
    RMP_CURRENCY = cfg.currency;
    RMP_FIO_TOKEN_PATH = cfg.payments.fio.tokenFile;
    RMP_FLEXIBLE_FEES = builtins.toJSON cfg.flexibleFees;
  } // optionalAttrs (cfg.feeAccount != null) {
    RMP_FEE_ACCOUNT = cfg.feeAccount;
  } // optionalAttrs (cfg.mail.replyTo != null) {
    RMP_MAIL_REPLY_TO = cfg.mail.replyTo;
  } // optionalAttrs (cfg.appRoot != null) {
    RMP_APPROOT = cfg.appRoot;
  };
  srv = name: {
    ExecStart = "${rememberportalPackage}/bin/${name}";
    WorkingDirectory = stateDir;
    User = user;
    Group = group;
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

        appRoot = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "https://m.example.com";
          description = "Root application URL, required for reminders";
        };

        mail = {
          sendmail = mkOption {
            type = types.str;
            default = "/run/current-system/sw/bin/sendmail";
            description = "Path to sendmail binary (see also: msmtp)";
          };

          from = mkOption {
            type = types.str;
            default = "noreply@example.com";
            description = "Email address to use in From: header";
          };

          replyTo = mkOption {
            type = types.str;
            default = "admin@example.com";
            description = "Email address to use in Reply-To: header";
          };
        };

        currency = mkOption {
          type = types.str;
          default = "BTC";
          description = "Membership fees currency";
        };

        flexibleFees = mkOption {
          type = types.bool;
          default = false;
          description = ''
            When flexible fees are enabled, each user can choose membership fee amount they'll be
            charged, provided that the amount is greater or equal to the basic fee.
          '';
        };

        port = mkOption {
          type = types.port;
          default = 13337;
          description = "Port number on which the application listens. Note that it always listens on loopback interface because https proxy in front of it is required.";
        };

        payments.fio.tokenFile = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "/run/keys/rememberportal-fio-token";
          description = "A file containing the token to FIO bank API.";
        };

        feeAccount = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "2900086515/2010";
          description = "Account number for membership fees, used in email reminders";
        };

        reminders = mkOption {
          type = types.bool;
          default = false;
          description = "Whether to send email reminders";
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

        rememberportal-sync-fio = mkIf (cfg.payments.fio.tokenFile != null) {
          description = "FIO payments sync task";
          environment = env;
          after = [ "rememberportal.service" ];
          requires = [ "rememberportal.service" ];
          serviceConfig = srv "rememberportal-sync-fio";
        };

        rememberportal-send-reminders = mkIf cfg.reminders {
          description = "Send email reminders to members with missing payments";
          environment = env;
          after = [ "rememberportal.service" ];
          requires = [ "rememberportal.service" ];
          serviceConfig = srv "rememberportal-send-reminders";
        };
      };

      systemd.timers = {
        rememberportal-create-fees = {
          description = "Periodically insert new membership fees";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = mkDefault "*-*-1 23:00:00";
            Persistent = true;
          };
        };
        rememberportal-sync-fio = mkIf (cfg.payments.fio.tokenFile != null) {
          description = "Periodically download payment information from FIO bank API";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = mkDefault "hourly";
            Persistent = true;
          };
        };
        rememberportal-send-reminders = mkIf cfg.reminders {
          description = "Periodically remind members about missing payments";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = mkDefault "*-*-17 16:45:00";
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
