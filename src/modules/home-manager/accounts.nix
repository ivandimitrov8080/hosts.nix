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
            "${pkgs.uutils-coreutils-noprefix}/bin/cat"
            "/home/ivand/.wg_cred"
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
            "${pkgs.uutils-coreutils-noprefix}/bin/cat"
            "/home/ivand/.wg_cred"
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
          passwordCommand = "${pkgs.uutils-coreutils-noprefix}/bin/cat /home/ivand/.wg_cred";
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
          # TODO: make emacs fn that opens the home-manager docs info from json and displays it
          notmuch.enable = true;
          offlineimap.enable = true;
          imapnotify = {
            enable = true;
            boxes = [
              "INBOX"
              "wrk"
            ];
            onNotify = "offlineimap";
            onNotifyPost = "notmuch new";
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
        afew
      ];
    };
    pimsync.enable = true;
  };
  programs = {
    notmuch = {
      enable = true;
      hooks = {
        postNew = "afew --tag --new";
      };
      new = {
        tags = [ "new" ];
      };
      search.excludeTags = [
        "trash"
        "spam"
      ];
    };
    aerc.enable = true;
    pimsync.enable = true;
    msmtp.enable = true;
    khal.enable = true;
    afew = {
      enable = true;
      extraConfig =
        let
          spammers = [
            "linkedin.com"
            "grafana.com"
            "bg.econcast.net"
            "ticketportal.cz"
            "uber.com"
            "brightdata.com"
            "cal.com"
            "rkc.edu"
            "office1.bg"
            "github.com"
          ];
        in
        ''
          [SpamFilter]

          [Filter.0]
          message = Delete all messages from spammers
          query = ${builtins.concatStringsSep " " (map (x: "from:*@${x}") spammers)}
          tags = +spam;-inbox;-unread

          [InboxFilter]
        '';
    };
  };
}
