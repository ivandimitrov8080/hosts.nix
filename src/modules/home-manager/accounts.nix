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
        khal = {
          enable = true;
          color = "light red";
          addresses = [ "ivan@idimitrov.dev" ];
          collections = [ "770000ed-6b2a-4dec-9534-f12bd3f8a482" ];
        };
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
            showSignature = "append";
          };
          gpg = {
            encryptByDefault = true;
            signByDefault = true;
            key = "ED7A E641 69C1 DB37 F48D  68A7 1C27 6C0A 3909 B508";
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
              signature-file = builtins.toFile "signature.txt" signature.text;
            };
          };
          notmuch.enable = true;
          offlineimap.enable = true;
          imapnotify = {
            enable = true;
            boxes = [
              "INBOX"
              "wrk"
            ];
            onNotify = "offlineimap";
            onNotifyPost = "notmuch new && notify-send 'New mail arrived'";
          };
        };
      };
    };
  };
  services = {
    imapnotify = {
      enable = true;
      path = with pkgs; [
        offlineimap
        notmuch
        libnotify
        pass
      ];
    };
    pimsync.enable = true;
  };
  programs = {
    notmuch.enable = true;
    aerc.enable = true;
    pimsync.enable = true;
    msmtp.enable = true;
    khal.enable = true;
  };
}
