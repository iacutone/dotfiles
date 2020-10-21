(use-package try
    :ensure t)

(use-package which-key
    :ensure t 
    :config
    (which-key-mode))

(require 'evil)
(evil-mode 1)

(define-key evil-ex-map "b " 'helm-mini)
(define-key evil-ex-map "e" 'helm-find-files)
(define-key evil-ex-map "g" 'helm-projectile-grep)
(define-key evil-ex-map "f" 'helm-projectile-find-file)

(define-key evil-ex-map "m" 'magit-blame)

(require 'doom-themes)

(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'doom-molokai t)
(doom-themes-visual-bell-config)
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

(require 'doom-modeline)
(doom-modeline-mode 1)

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

(use-package ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

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

(require 'helm)

(setq-default helm-M-x-fuzzy-match t)
(global-set-key "\C-x\C-m" 'helm-M-x)
(global-set-key "\C-c\C-m" 'helm-M-x)
(define-key evil-ex-map "x" 'helm-M-x)

(define-key evil-ex-map "b " 'helm-mini)
(define-key evil-ex-map "e" 'helm-find-files)

(require 'helm-projectile)
(define-key evil-ex-map "g" 'helm-projectile-grep)
(define-key evil-ex-map "f" 'helm-projectile-find-file)

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

(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org")
      (deft-directory "~/Dropbox/orgfiles/roam"))

(use-package org-journal
      :bind
      ("C-c n j" . org-journal-new-entry)
      :custom
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-dir "~/Dropbox/orgfiles/roam")
      (org-journal-date-format "%A, %d %B %Y"))

; (require 'company-org-roam)
; (push 'company-org-roam company-backends)

(use-package org-roam
  :ensure t
  :hook ((after-init . org-roam-mode))
  :custom
  (org-roam-directory "~/Dropbox/orgfiles/roam")
  (org-roam-index-file "index.org")
  :bind (:map org-roam-mode-map
         (("C-c m l" . org-roam)
          ("C-c m F" . org-roam-find-file)
          ("C-c m r" . org-roam-find-ref)
          ("C-c m ." . org-roam-find-directory)
          ("C-c m >" . zp/org-roam-find-directory-testing)
          ("C-c m d" . org-roam-dailies-map)
          ("C-c m j" . org-roam-jump-to-index)
          ("C-c m b" . org-roam-switch-to-buffer)
          ("C-c m g" . org-roam-graph))
         :map org-mode-map
         (("C-c m i" . org-roam-insert)))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain
           (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n\n"
           :unnarrowed t))))

(add-hook 'before-save-hook #'zp/org-set-last-modified)

;;--------------------------
;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
;;--------------------------

(defun zp/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.
   When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun zp/org-has-time-file-property-p (property &optional anywhere)
  "Return the position of time file PROPERTY if it is defined.
   As a special case, return -1 if the time file PROPERTY exists but
   is not defined."
  (when-let ((pos (zp/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))

(defun zp/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.
   When ANYWHERE is non-nil, search beyond the preamble.
   If the position of the file PROPERTY has already been computed,
   it can be passed in POS."
  (when-let ((pos (or pos
                      (zp/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun zp/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (zp/org-set-time-file-property "LAST_MODIFIED")))

(setq org-roam-completion-system 'helm)

(use-package org-roam-server
  :ensure t
  :bind (:map org-roam-mode-map
         (("C-c m G" . org-roam-server-mode)))
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 9999
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(require 'rtags)
(require 'cmake-ide)
(cmake-ide-setup)

(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

(use-package rtags
  :ensure t
  :hook (c++-mode . rtags-start-process-unless-running)
  :config (setq rtags-completions-enabled t
		rtags-path "~/dotfiles/emacs.d/rtags/src/rtags.el"
		rtags-rc-binary-name "~/dotfiles/emacs.d/rtags/bin/rc"
		rtags-use-helm t
		rtags-rdm-binary-name "~/dotfiles/emacs.d/rtags/bin/rdm")
  :bind (("C-c E" . rtags-find-symbol)
  	 ("C-c e" . rtags-find-symbol-at-point)
  	 ("C-c O" . rtags-find-references)
  	 ("C-c o" . rtags-find-references-at-point)
  	 ("C-c s" . rtags-find-file)
  	 ("C-c v" . rtags-find-virtuals-at-point)
  	 ("C-c F" . rtags-fixit)
  	 ("C-c f" . rtags-location-stack-forward)
  	 ("C-c b" . rtags-location-stack-back)
  	 ("C-c n" . rtags-next-match)
  	 ("C-c p" . rtags-previous-match)
  	 ("C-c P" . rtags-preprocess-file)
  	 ("C-c R" . rtags-rename-symbol)
  	 ("C-c x" . rtags-show-rtags-buffer)
  	 ("C-c T" . rtags-print-symbol-info)
  	 ("C-c t" . rtags-symbol-type)
  	 ("C-c I" . rtags-include-file)
  	 ("C-c S" . rtags-get-include-file-for-symbol)
	 ))

(setq rtags-display-result-backend 'helm)

(use-package ledger-mode
  :ensure t
  :defer t
  :init
  )

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
