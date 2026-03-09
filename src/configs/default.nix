{ inputs }:
let
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };
  nixosModules = inputs.self.nixosModules.default;
  hardwareConfigurations = import ../constants;
  allowUnfree =
    { lib, ... }:
    {
      nixpkgs.config = {
        allowUnfree = lib.mkForce false;
      };
      nixpkgs.config.allowUnfreePredicate =
        pkg:
        builtins.elem (lib.getName pkg) [
          "steam"
          "steam-original"
          "steam-unwrapped"
          "steam-run"
          "discord"
        ];
    };
in
rec {
  nova = inputs.nixpkgs.lib.nixosSystem {
    modules =
      (with nixosModules; [
        default
        rest
        nova-module
      ])
      ++ [ hardwareConfigurations.nova ];
  };
  gaming = nova.extendModules {
    modules = with nixosModules; [
      gamingModule
      allowUnfree
    ];
  };
  music = nova.extendModules {
    modules = [
      {
        meta.music.enable = true;
        boot.kernelPackages = pkgs.linuxPackages_zen;
      }
    ];
  };
  vps = inputs.nixpkgs.lib.nixosSystem {
    modules = with nixosModules; [
      vpsadminosModule
      default
      mail
      nginx
      {
        nixpkgs.hostPlatform = "x86_64-linux";
        imports = with inputs; [
          vpsadminos.nixosConfigurations.containerUnstable
        ];
        webshite.enable = true;
        _module.args.system = system;
        services = {
          crowdsec = {
            enable = true;
            localConfig = {
              acquisitions = [
                {
                  journalctl_filter = [
                    "_SYSTEMD_UNIT=sshd.service"
                  ];
                  labels = {
                    type = "syslog";
                  };
                  source = "journalctl";
                }
                {
                  journalctl_filter = [
                    "_TRANSPORT=kernel"
                  ];
                  labels = {
                    type = "syslog";
                  };
                  source = "journalctl";
                }
              ];
            };
            settings = {
              general = {
                api.server = {
                  enable = true;
                  listen_uri = "127.0.0.1:8080";
                };
              };
              lapi.credentialsFile = "/etc/crowdsec/lapi.yaml";
            };
          };
          prometheus = {
            enable = true;
            scrapeConfigs = [
              {
                job_name = "crowdsec";
                static_configs = [ { targets = [ "127.0.0.1:6060" ]; } ];
              }
            ];
          };
          loki = {
            enable = true;
            configuration = {
              auth_enabled = false;
              server = {
                http_listen_address = "127.0.0.1";
                http_listen_port = 3100;
              };
              common = {
                path_prefix = "/var/lib/loki";
                storage = {
                  filesystem = {
                    chunks_directory = "/var/lib/loki/chunks";
                    rules_directory = "/var/lib/loki/rules";
                  };
                };
                replication_factor = 1;
                ring = {
                  kvstore = {
                    store = "inmemory";
                  };
                };
              };
              schema_config = {
                configs = [
                  {
                    from = "2024-01-01";
                    store = "tsdb";
                    object_store = "filesystem";
                    schema = "v13";
                    index = {
                      prefix = "index_";
                      period = "24h";
                    };
                  }
                ];
              };
            };
          };
          grafana = {
            enable = true;
            settings = {
              server = {
                http_addr = "127.0.0.1";
                http_port = 34321;
                domain = "grafana.idimitrov.dev";
                root_url = "https://grafana.idimitrov.dev/";
                serve_from_sub_path = false;
              };
              # serve this only on wireguard interface
              security.secret_key = "SW2YcwTIb9zpOOhoPsMm";
            };
          };
          alloy.enable = true;
        };
        systemd.services.alloy.serviceConfig.SupplementaryGroups = [ "nginx" ];
        environment.etc."alloy/config.alloy".text = ''
          loki.write "local" {
            endpoint { url = "http://127.0.0.1:3100/loki/api/v1/push" }
          }

          loki.relabel "journal_labels" {
            forward_to = []

            rule {
              source_labels = ["__journal__systemd_unit"]
              target_label  = "unit"
            }

            rule {
              source_labels = ["__journal__systemd_unit"]
              regex         = "nginx\\.service"
              target_label  = "job"
              replacement   = "nginx-journal"
            }
          }

          loki.source.journal "journald_all" {
            forward_to    = [loki.write.local.receiver]
            relabel_rules = loki.relabel.journal_labels.rules

            labels = {
              job = "journald",
            }
          }
          loki.source.file "nginx_access" {
            targets = [
              {
                __path__   = "/var/log/nginx/access.json.log",
                job        = "nginx",
                log_source = "file",
                log_type   = "access",
              },
            ]

            forward_to = [loki.write.local.receiver]
          }
        '';
        users.users = {
          loki.extraGroups = [ "systemd-journal" ];
          nginx.extraGroups = [ "acme" ];
        };
      }
    ];
  };
}
