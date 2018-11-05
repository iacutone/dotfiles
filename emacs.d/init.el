(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq evil-want-C-i-jump nil)
(defalias 'list-buffers 'ibuffer) ; make ibuffer default

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(tool-bar-mode -1)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(org-babel-load-file (expand-file-name "~/dotfiles/emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-ref writeroom-mode olivetti org-pomodoro wc-mode grab-mac-link ## google-this helm-google gnugo xpm buffer-stack notmuch org-projectile org-mac-link rvm dumb-jump robe rspec-mode eyebrowse helm-dash helm-ag helm-projectile evil-rails calfw-ical calfw-org calfw powerline org-gcal web-mode expand-region beacon elpy zenburn-theme which-key use-package try swiper org-bullets helm evil auto-complete ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
