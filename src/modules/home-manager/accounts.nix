{ pkgs, ... }:
{
  accounts = {
    calendar = {
      accounts.ivand = {
        khal = {
          enable = true;
          color = "light green";
          type = "discover";
        };
        pimsync = {
          enable = true;
          extraPairDirectives = [
            {
              name = "collections";
              params = [ "all" ];
            }
          ];
        };
        remote = {
          passwordCommand = [
            "${pkgs.pass}/bin/pass"
            "vps/mail.idimitrov.dev/ivan@idimitrov.dev"
          ];
          type = "caldav";
          url = "https://dav.idimitrov.dev";
          userName = "ivan@idimitrov.dev";
        };
      };
    };
    contact = {
      accounts.ivand = {
        khard = {
          enable = true;
          type = "discover";
        };
        pimsync = {
          enable = true;
          extraPairDirectives = [
            {
              name = "collections";
              params = [ "all" ];
            }
          ];
        };
        remote = {
          type = "carddav";
          url = "https://dav.idimitrov.dev";
          userName = "ivan@idimitrov.dev";
          passwordCommand = [
            "${pkgs.pass}/bin/pass"
            "vps/dav.idimitrov.dev/ivan@idimitrov.dev"
          ];
        };
      };
    };
    email = {
      accounts = {
        "ivan@idimitrov.dev" = rec {
          primary = true;
          realName = "Ivan Kirilov Dimitrov";
          address = "ivan@idimitrov.dev";
          userName = address;
          passwordCommand = "pass vps/mail.idimitrov.dev/ivan@idimitrov.dev";
          msmtp = {
            enable = true;
            extraConfig = {
              auth = "login";
            };
          };
          signature = {
            text = ''
              Ivan Dimitrov
              Software Developer
              ivan@idimitrov.dev
            '';
          };
          gpg = {
            encryptByDefault = true;
            signByDefault = true;
            key = "C565 2E79 2A7A 9110 DFA7  F77D 0BDA D4B2 11C4 9294";
          };
          smtp = {
            host = "mail.idimitrov.dev";
            port = 465;
            authentication = "login";
          };
          imap = {
            host = "mail.idimitrov.dev";
            authentication = "login";
          };
          aerc = {
            enable = true;
            smtpAuth = "login";
            imapAuth = "auth";
            extraAccounts = {
              default = "INBOX";
              restrict-delete = true;
            };
          };
          offlineimap.enable = true;
        };
      };
    };
  };
}
