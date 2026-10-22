;;; rss-feeds.el --- RSS feeds -*- lexical-binding: t; -*-

;;; Commentary:
;; Elfeed feeds and entry handling.

;;; Code:

(require 'elfeed)

(defun rss-feeds-init ()
  "Configure Elfeed."
  (setq elfeed-feeds
        '(("https://rss.arxiv.org/atom/cs.AI" cs ai)
          ("https://rss.arxiv.org/atom/cs.CE" cs eng fin sci)
          ("https://rss.arxiv.org/atom/cs.CY" cs soc)
          ("https://www.technologyreview.com/topic/artificial-intelligence/feed" cs ai)
          ("https://cvefeed.io/rssfeed/severity/high.atom" cs cve)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCODHrzPMGbNv67e84WDZhQQ" yt fern)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" yt mentaloutlaw)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKTwTio3tOv713CnIcxyLhA" yt dungeonsoup)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" yt 3b1b)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q" yt kurzgesagt)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1D3yD4wlPMico0dss264XA" yt nileblue)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCFhXFikryT4aFcLkLw2LBLA" yt nilered)))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread)))

(unless noninteractive
  (rss-feeds-init))

(provide 'rss-feeds)
;;; rss-feeds.el ends here
