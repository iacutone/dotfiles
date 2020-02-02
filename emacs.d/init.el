(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq evil-want-C-i-jump nil)
(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq org-agenda-include-diary t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(global-set-key (kbd "<f6>") 'org-capture)

(org-babel-load-file (expand-file-name "~/dotfiles/emacs.d/myinit.org"))
(org-babel-load-file (expand-file-name "~/dotfiles/emacs.d/org-setup.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("203fe0858c2018058526eff9887b06facf5044a94cf8af4dbf66bd16057d28f1" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(org-agenda-custom-commands
   (quote
    (("g" "Weekly Goals Review"
      ((tags "Goal+TODO=\"TODO\"+INTERVAL=\"\""
             ((org-agenda-overriding-header "Actions that don't contribute to a goal yet")
              (org-agenda-overriding-header "")))
       (tags "INTERVAL=\"short\"+TODO=\"TODO\""
             ((org-agenda-overriding-header "Short Term Goals")))
       (tags "INTERVAL=\"medium\"+TODO=\"TODO\""
             ((org-agenda-overriding-header "Medium Term Goals")))
       (tags "INTERVAL=\"long\"+TODO=\"TODO\""
             ((org-agenda-overriding-header "Long Term Goals")))
       (tags "INTERVAL=\"epic\""
             ((org-agenda-overriding-header "Epic Goals"))))
      nil nil)
     ("d" "Today's Overview"
      ((tags-todo "URGENT"
                  ((org-agenda-files
                    (quote
                     ("~/Dropbox/orgfiles/work.org" "~/Dropbox/orgfiles/life.org")))))
       (tags "-Goal+TODO=\"DONE\"" nil)
       (tags "-Goal-SHAWN+TODO=\"TODO\"-DEADLINE>=\"<+7d>\"" nil)
       (todo "CANCELED"
             ((org-agenda-files
               (quote
                ("~/Dropbox/orgfiles/work.org")))))
       (agenda ""
               ((org-agenda-files
                 (quote
                  ("~/Dropbox/orgfiles/birthdays.org" "~/Dropbox/orgfiles/gcal.org" "~/Dropbox/orgfiles/life.org" "~/Dropbox/orgfiles/work.org"))))))
      nil nil)
     ("s" "Talking Points with Shawn" tags "SHAWN" nil))))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (dot . t) (ruby . t) (shell . t))))
 '(org-clock-into-drawer "CLOCKING")
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-export-backends (quote (ascii beamer html icalendar latex odt)))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox --new-tab %s")
     ("\\.pdf\\'" . default))))
 '(org-habit-show-habits-only-for-today t)
 '(org-log-into-drawer t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets (quote ((org-agenda-files :level . 1))))
 '(org-refile-use-outline-path (quote file))
 '(org-track-ordered-property-with-tag t)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (rtags doom-modeline doom doom-themes anki-editor bbdb synosaurus deft helm-org-rifle evil-ledger hyperbole rinari poet-theme chruby ruby-electric solarized-theme better-defaults shackle tomatinho pdf-tools org-trello org-ref writeroom-mode olivetti org-pomodoro wc-mode grab-mac-link ## google-this helm-google gnugo xpm buffer-stack notmuch org-projectile rvm dumb-jump robe rspec-mode eyebrowse helm-dash helm-ag helm-projectile evil-rails calfw-ical calfw-org calfw powerline org-gcal web-mode expand-region beacon elpy zenburn-theme which-key use-package try swiper org-bullets helm evil auto-complete ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
