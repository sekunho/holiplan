{ config, pkgs, lib, holiplan, ... }:
  let
    cfg = config.services.holiplan;
  in with lib; {
    options = {
      services.holiplan = {
        enable = mkOption {
          default = false;
          type = with types; bool;
          description = "Starts the holiplan server";
        };

        port = mkOption {
          default = 3000;
          type = with types; integer;
          description = "Port number for holiplan to bind to";
        };

        dbHost = mkOption {
          default = "localhost";
          type = with types; str;
          description = "Host of the database server";
        };

        dbPort = mkOption {
          default = 5432;
          type = with types; number;
          description = "Port number of the database server";
        };

        dbUser = mkOption {
          type = with types; str;
          description = "Database user";
        };

        dbPasswordFile = mkOption {
          default = "";
          type = with types; str;
          description = "File path to the database's password file";
        };

        dbCACertFile = mkOption {
          default = "";
          type = with types; str;
          description = "File path to the database's password file";
        };
      };
    };

    config = mkIf cfg.enable {
      systemd.services.holiplan = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        description = "Start the holiplan server";

        environment = mkMerge [
          {
            APP__PORT = "${toString cfg.port}";
            PG__DBNAME = "${cfg.dbName}";
            PG__HOST = "${cfg.dbHost}";
            PG__PORT = "${toString cfg.dbPort}";
            PG__USER = "${cfg.dbUser}";
          }

          (mkIf cfg.dbPasswordFile != "" {
            PG__PASSWORD_FILE = "${cfg.dbPasswordFile}";
          })

          (mkIf cfg.dbCACertFile != "" {
            PG__CA_CERT_FILE = "${cfg.dbCACertFile}";
          })
        ];

        serviceConfig = {
          Type = "simple";
          ExecStart = "${holiplan}/bin/holiplan";
        };
      };

      environment.systemPackages = [ holiplan ];
    };
  }
