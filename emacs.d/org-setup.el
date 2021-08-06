(use-package org)
; (use-package org-ellipsis)

(use-package visual-fill-column
  :defer t
  :hook (org-mode . ei/org-mode-visual-fill))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(global-set-key (kbd "C-c c")
       'org-capture)

(global-set-key (kbd "C-c b")
       'org-agenda-switch-to)

(setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"
			     "~/Dropbox/orgfiles/work.org"
			     "~/Dropbox/orgfiles/life.org"
			     "~/Dropbox/orgfiles/goals.org"
			     "~/Dropbox/orgfiles/birthdays.org"
			     "~/Dropbox/orgfiles/entertainment.org"))
; Set key combos
(define-key global-map "\C-ca" 'org-agenda)

(setq org-capture-templates
      '(("w" "Work To Do" entry (file+headline "~/Dropbox/orgfiles/work.org" "To Do Items")
	  (file "~/dotfiles/emacs.d/template-todo.txt"))
        ("r" "Weekly Review" entry (file+datetree "~/Dropbox/orgfiles/review.org")
	  (file "~/dotfiles/emacs.d/template-review.txt"))
        ("c" "Contact" entry (file+headline "~/Dropbox/orgfiles/contact.org" "Contacts") 
	  (file "~/dotfiles/emacs.d/template-contact.txt"))
        ("l" "Life") 
          ("ln" "Note" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Notes")
	    (file "~/dotfiles/emacs.d/template-note.txt"))
	  ("lb" "Blog Idea" entry (file+headline "~/Dropbox/orgfiles/blog.org" "Blog Idea")
	     "* Blog %?\n%T")
	  ("lf" "Food" entry (file+headline "~/Dropbox/orgfiles/life.org" "Food")
	    (file "~/dotfiles/emacs.d/template-todo.txt"))
	  ("ly" "Youtube" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Youtube")
	    (file "~/dotfiles/emacs.d/template-youtube.txt"))
	  ("lt" "Life To Do" entry (file+headline "~/Dropbox/orgfiles/life.org" "To Do Items")
	    (file "~/dotfiles/emacs.d/template-todo.txt"))
	  ("lh" "Home" entry (file+headline "~/Dropbox/orgfiles/life.org" "Home")
	    (file "~/dotfiles/emacs.d/template-todo.txt"))
	  ("lm" "Movie" entry (file+headline "~/Dropbox/orgfiles/entertainment.org" "Movies")
	    (file "~/dotfiles/emacs.d/template-movie.txt"))
	  ("lu" "Music" entry (file+headline "~/Dropbox/orgfiles/entertainment.org" "Music")
	    (file "~/dotfiles/emacs.d/template-music.txt"))
	  ("lg" "Gratitude" entry (file+datetree "~/Dropbox/orgfiles/gratitude.org")
	    (file "~/dotfiles/emacs.d/template-gratitude.txt"))
	  ("lr" "Recipe" entry (file+headline "~/Dropbox/orgfiles/recipes.org" "Recipes")
	    (file "~/dotfiles/emacs.d/template-recipe.txt"))
	  ("lj" "Journal" entry (file+datetree "~/Dropbox/orgfiles/journal.org") 
	    "** %^{Title}")
	  ("lw" "Good Bad" entry (file+datetree "~/Dropbox/orgfiles/journal.org") 
	    (file "~/dotfiles/emacs.d/template-good-bad.txt"))
	  ("ld" "Decision" entry (file+headline "~/Dropbox/orgfiles/decision.org" "Decisions") 
	    (file "~/dotfiles/emacs.d/template-decision-journal.txt"))
	  ("la" "Travel" entry (file+headline "~/Dropbox/orgfiles/travel.org" "Travel") 
	    (file "~/dotfiles/emacs.d/template-travel.txt"))
	  ("lo" "Book" entry (file+headline "~/Dropbox/orgfiles/book.org" "Book") 
	    (file "~/dotfiles/emacs.d/template-book.txt"))
	("g" "Goals") 
	  ("ge" "Epic goals" entry (file+headline "~/Dropbox/orgfiles/goals.org" "Epic Goals") 
	    (file "~/dotfiles/emacs.d/template-goal.txt") :empty-lines-after 1)
	  ("gl" "Long term goal (2-5 years from now)" entry (file+headline "~/Dropbox/orgfiles/goals.org" "Long term goals") 
	    (file "~/dotfiles/emacs.d/template-goal.txt") :empty-lines-after 1) 
	  ("gm" "Medium term goal (6 months up to 2 years)" entry (file+headline "~/Dropbox/orgfiles/goals.org" "Medium term goals") 
	    (file "~/dotfiles/emacs.d/template-goal.txt") :empty-lines-after 1) 
	  ("gs" "Short term goals (next 6 months)" entry (file+headline "~/Dropbox/orgfiles/goals.org" "Short term goals") 
	    (file "~/dotfiles/emacs.d/template-goal.txt") :empty-lines-after 1)))

(defun make-capture-frame ()
 "Create a new frame and run org-capture."
 (interactive)
 (make-frame '((name . "capture")))
 (select-frame-by-name "capture")
 (delete-other-windows)
 (org-capture))

(setq org-log-done 'time)
(setq org-log-into-drawer t)

; http://doc.norang.ca/org-mode.html
(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)
(setq org-agenda-span 'day)

(defun insert-current-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date '+%Y-%m-%d %A')")))

(global-set-key (kbd "C-c d") 'insert-current-date)
 
(global-set-key (kbd "C-c id") 'copy-id-to-clipboard)

;(add-hook 'org-mode-hook
;  (lambda ()
;    (add-hook 'before-save-hook 'org-add-ids-to-headlines-in-file nil 'local)))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1))))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defun ei/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
  
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (ledger . t)))
  
(setq org-confirm-bable-evaluate nil)

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-directory (expand-file-name "~/Dropbox/orgfiles/roam"))
  (org-roam-setup)
  (require 'org-roam-protocol))

"Before first headline at position 1 in buffer <file_name>.org"

(use-package ledger-mode
  :defer t
  :init
  )

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
