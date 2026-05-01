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
              signature-file = builtins.toFile "signature.txt" signature.text;
            };
          };
          offlineimap.enable = true;
        };
      };
    };
  };
  programs = {
    aerc = {
      enable = true;
      templates = {
        wrk = ''
          To:
          Subject: Application — Software Developer — Ivan Dimitrov
          {{.Attach "/home/ivand/doc/cv.pdf"}}
          Hello [NAME],

          I’m applying for the Software Developer role. I’m a developer focused on web development, and I’d love to help
          with building exciting new things.

          Links:
          - GitHub: https://github.com/ivandimitrov8080
          - Site: https://idimitrov.dev
          - Upwork: https://www.upwork.com/freelancers/~014fabab43ea6d5131

          I’ve attached my CV. If you’d like, I can share a couple of relevant projects based on your needs.

          Best regards,
          {{.Signature}}
        '';
      };
      extraBinds = {
        messages = {
          w = ":compose -T wrk<Enter>";
        };
      };
    };
    pimsync.enable = true;
    msmtp.enable = true;
    khal.enable = true;
  };
}
