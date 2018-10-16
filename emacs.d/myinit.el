(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package try
    :ensure t)

(use-package which-key
    :ensure t 
    :config
    (which-key-mode))

(require 'evil)
(evil-mode 1)
(setq evil-want-C-i-jump nil)

(use-package evil-rails
:ensure t
:config 
)

(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-word-1))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(global-set-key (kbd "C-c c")
       'org-capture)

(global-set-key (kbd "C-c b")
       'org-agenda-switch-to)

(setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"
                             "~/Dropbox/orgfiles/todo.org"))
; Set key combos
(define-key global-map "\C-ca" 'org-agenda)

(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Notes")
	 "* Note %?\n%T")
	("l" "Link" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
	 "* %? %^L %^g \n%T" :prepend t)
	 ("y" "Youtube" entry (file+headline "~/Dropbox/orgfiles/youtube.org" "Youtube")
	  "* Note %?\n%T")
	("t" "To Do Item" entry (file+headline "~/Dropbox/orgfiles/todo.org" "To Do Items")
	"* TODO %?\n%T")))

(setq org-log-done 'time)

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
    ))
(global-set-key (kbd "M-p") 'ace-window)

(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

(use-package helm-projectile
:ensure t
:config)

(require 'helm-projectile)
(helm-projectile-on)

(use-package helm-dash
:ensure t
:config)

(use-package helm-ag
:ensure t
:config)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "s-f") #'helm-projectile-ag)
(global-set-key (kbd "s-t") #'helm-projectile-find-file-dwim)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package inf-ruby
:ensure t
:config
)

(use-package robe
:ensure t
:config
)
(add-hook 'ruby-mode-hook 'robe-mode)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(use-package rspec-mode
:ensure t
:config
)
(require 'rspec-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package rvm
:ensure t
:config
)
(require 'rvm)
(rvm-use-default)

(use-package elpy
:ensure t
:config 
(elpy-enable))

(global-hl-line-mode t)

; flashes the cursor's line when you scroll
(use-package beacon
:ensure t
:config
(beacon-mode 1)
(setq beacon-color "#666600")
)

; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
:ensure t
:config 
(global-set-key (kbd "C-=") 'er/expand-region))

;; change mode-line color by evil state
(eval-when-compile (require 'cl))
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))

(add-hook 'post-command-hook
       (lambda ()
         (let ((color (cond ((minibufferp) default-color)
                            ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                            ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                            ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                            (t default-color))))
           (set-face-background 'mode-line (car color))
           (set-face-foreground 'mode-line (cdr color))))))

;(defun load-if-exists (f)
;  (if file-readable-p f)
;    (load-file f)))

; (load-if-exists "~/Dropbox/something.el")

(exec-path-from-shell-initialize)
(when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))

(use-package org-gcal
  :ensure t
  :config
  (setq org-gcal-client-id (exec-path-from-shell-copy-env "WORK_GMAIL_CAL_CLIENT_ID")
	org-gcal-client-secret (exec-path-from-shell-copy-env "WORK_GMAIL_CAL_CLIENT_SECRET")
	org-gcal-file-alist '(("eric.iacutone@fracturedatlas.org" .  "~/Dropbox/orgfiles/gcal.org"))))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-fetch)))

(defun workcal ()
    (interactive)
    (cfw:open-org-calendar))

(use-package calfw
  :ensure t
  :config
  (require 'calfw) 
  (require 'calfw-org))

(use-package eyebrowse
:ensure t
:config 
)

(eyebrowse-mode t)
(eyebrowse-setup-opinionated-keys)

(require 'powerline)
(powerline-default-theme)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm)
  :ensure)
(dumb-jump-mode)

(use-package org-mac-link
  :ensure t
  :config)
(add-hook 'org-mode-hook (lambda () 
  (define-key org-mode-map (kbd "C-c g") 'org-mac-chrome-insert-frontmost-url)))


