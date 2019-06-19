(use-package try
    :ensure t)

(use-package which-key
    :ensure t 
    :config
    (which-key-mode))

(require 'evil)
(evil-mode 1)
(setq evil-want-C-i-jump nil)

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

(require 'helm-config)
(helm-mode 1)

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
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(require 'chruby)
(chruby "2.5.3")
(require 'rinari)

(use-package evil-rails
:ensure t
:config 
)

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

(exec-path-from-shell-initialize)
(when (memq window-system '(mac ns x))
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

(setq hugo-base-dir "~/blog/"
      hugo-buffer "*hugo*")

(defun hugo-new-post ()
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         (filename (concat "post/"
		    (read-from-minibuffer "Filename: "
		     (replace-regexp-in-string "-\\.md" ".md"
		      (concat (downcase
			       (replace-regexp-in-string "[^a-z0-9]+" "-"
				title))
                                                           ".md")))))
         (path (concat hugo-base-dir "content/" filename)))

    (if (file-exists-p path)
        (message "File already exists!")
      (hugo-command "new" filename)
      (find-file path)
      (hugo-replace-key "title" title)
      (goto-char (point-max))
      (save-buffer))))

(defun hugo-publish ()
  (interactive)
  (let* ((default-directory (concat (expand-file-name hugo-base-dir) "/")))
    (when (call-process "bash" nil hugo-buffer t  "~/scripts/deploy_blog.sh")
      (message "New blog post published"))))

(defun hugo-command (&rest args)
  (let ((default-directory (expand-file-name hugo-base-dir)))
    (apply 'call-process "hugo" nil hugo-buffer t args)))

(defun hugo-replace-key (key val)
  (save-excursion
    (goto-char (point-min))
    ; quoted value
    (if (and (re-search-forward (concat key " = \"") nil t)
               (re-search-forward "[^\"]+" (line-end-position) t))
        (or (replace-match val) t) ; ensure we return t
      ; unquoted value
      (when (and (re-search-forward (concat key " = ") nil t)
                 (re-search-forward ".+" (line-end-position) t))
        (or (replace-match val) t)))))

(defun hugo-undraft ()
  (interactive)
  (when (and (hugo-replace-key "date" (iso-timestamp))
             (hugo-replace-key "draft" "false"))
    (save-buffer)
    (message "Removed draft status and updated timestamp")))

(defun iso-timestamp ()
  (concat (format-time-string "%Y-%m-%dT%T")
          ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
           (format-time-string "%z"))))

(defun hugo-server (&optional arg)
  (interactive "P")
  (let* ((default-directory (concat (expand-file-name hugo-base-dir) "/"))
         (proc (get-buffer-process hugo-buffer)))
    (if (and proc (process-live-p proc))
        (progn (interrupt-process proc)
               (message "Stopped Hugo server"))
      (start-process "hugo" hugo-buffer "hugo" "server")
      (message "Started Hugo server")
      (unless arg
        (browse-url "http://localhost:1313/")))))

(require 'wc-mode)
(require 'org-pomodoro)
(require 'writegood-mode)
(require 'olivetti)
(require 'writeroom-mode)
(global-set-key (kbd "<f12>") 'tomatinho)
(require 'hyperbole)
(require 'synosaurus)
; (require 'org-ref)

(require 'org-trello)

(require 'deadgrep)
(global-set-key (kbd "<f5>") #'deadgrep)

(require 'shackle)

(require 'better-defaults)

; (require 'org-ledger)

; (require 'org-brain)

(require 'helm-org-rifle)

(require 'deft)
(use-package deft
  :commands (deft)
  :config (setq deft-directory "~/Dropbox/deft"))
