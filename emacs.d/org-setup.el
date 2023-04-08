(use-package org)

(use-package visual-fill-column
  :defer t
  :hook (org-mode . ei/org-mode-visual-fill))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(global-set-key (kbd "C-c c")
       'org-capture)

(setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"
			     "~/Dropbox/orgfiles/work.org"
			     "~/Dropbox/orgfiles/life.org"
			     "~/Dropbox/orgfiles/goals.org"
			     "~/Dropbox/orgfiles/birthdays.org"
			     "~/Dropbox/orgfiles/entertainment.org"))

(setq org-capture-templates
      '(("w" "Work To Do" entry (file+headline "~/Dropbox/orgfiles/work.org" "To Do Items")
	  (file "~/dotfiles/emacs.d/template-todo.txt"))
        ("r" "Weekly Review" entry (file+datetree "~/Dropbox/orgfiles/review.org")
	  (file "~/dotfiles/emacs.d/template-review.txt"))
        ("c" "Contact" entry (file+headline "~/Dropbox/orgfiles/contact.org" "Contacts") 
	  (file "~/dotfiles/emacs.d/template-contact.txt"))
        ("l" "Life") 
	  ("la" "Travel" entry (file+headline "~/Dropbox/orgfiles/travel.org" "Travel") 
	    (file "~/dotfiles/emacs.d/template-travel.txt"))
	  ("lb" "Blog Idea" entry (file+headline "~/Dropbox/orgfiles/blog.org" "Blog Idea")
	     "* Blog %?\n%T")
	  ("ld" "Decision" entry (file+headline "~/Dropbox/orgfiles/decision.org" "Decisions") 
	    (file "~/dotfiles/emacs.d/template-decision-journal.txt"))
	  ("lf" "Food" entry (file+headline "~/Dropbox/orgfiles/life.org" "Food")
	    (file "~/dotfiles/emacs.d/template-todo.txt"))
	  ("lg" "Gratitude" entry (file+datetree "~/Dropbox/orgfiles/gratitude.org")
	    (file "~/dotfiles/emacs.d/template-gratitude.txt"))
	  ("lh" "Home" entry (file+headline "~/Dropbox/orgfiles/life.org" "Home")
	    (file "~/dotfiles/emacs.d/template-todo.txt"))
	  ("lj" "Journal" entry (file+datetree "~/Dropbox/orgfiles/journal.org") 
	    "** %^{Title}")
	  ("lm" "Movie" entry (file+headline "~/Dropbox/orgfiles/entertainment.org" "Movies")
	    (file "~/dotfiles/emacs.d/template-movie.txt"))
          ("ln" "Note" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Notes")
	    (file "~/dotfiles/emacs.d/template-note.txt"))
	  ("lo" "Book" entry (file+headline "~/Dropbox/orgfiles/entertainment.org" "Book") 
	    (file "~/dotfiles/emacs.d/template-book.txt"))
	  ("lr" "Recipe" entry (file+headline "~/Dropbox/orgfiles/recipes.org" "Recipes")
	    (file "~/dotfiles/emacs.d/template-recipe.txt"))
	  ("lt" "Life To Do" entry (file+headline "~/Dropbox/orgfiles/life.org" "To Do Items")
	    (file "~/dotfiles/emacs.d/template-todo.txt"))
	  ("lu" "Music" entry (file+headline "~/Dropbox/orgfiles/entertainment.org" "Music")
	    (file "~/dotfiles/emacs.d/template-music.txt"))
	  ("lw" "Good Bad" entry (file+datetree "~/Dropbox/orgfiles/journal.org") 
	    (file "~/dotfiles/emacs.d/template-good-bad.txt"))
	  ("ly" "Youtube" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Youtube")
	    (file "~/dotfiles/emacs.d/template-youtube.txt"))))

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-span 'day)

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
  :custom
  (org-roam-completion-everywhere t)
  :bind 
  (("C-c r c" . org-roam-capture)
   ("C-c r d" . org-roam-dailies-capture-today)
   ("C-c r i" . org-roam-node-insert)
   ("C-c r f" . org-roam-node-find))
  :config
  (setq org-roam-directory (expand-file-name "~/Dropbox/orgfiles/roam"))
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :target
                                      :unnarrowed t)
                                     ("r" "bibliography reference" plain
                                      "%?"
                                      :target
                                      (file+head
                                       "references/${citekey}.org"
                                       "#+title: ${title}\n")
                                      :unnarrowed t)))
	(org-roam-db-autosync-mode t))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-ref
  :ensure t
  :config
  (setq
   bibtex-completion-bibliography '("~/Zotero/bibtex.bib")
   bibtex-completion-notes-path "~/Documents/notes/references"
   bibtex-completion-pdf-field "file"
   bibtex-completion-pdf-open-function
   (lambda (fpath)
     (call-process "open" nil 0 nil fpath))))

(use-package ivy-bibtex
  :ensure t
  :after org-ref)

(use-package org-roam-bibtex
  :ensure t  
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind
  (("C-c r z" . orb-insert-link))
  :config
  (require 'org-ref))

(use-package ledger-mode
  :ensure t
  :defer t
  :init
  )

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(use-package org-super-agenda
    :ensure t
    :after org
    :defer t
    :config
    (org-agenda nil "u")
    )

(let ((org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODAY")  ; Items that have this TODO keyword
         (:name "Important"
                ;; Single arguments given alone
                :tag "bills"
                :priority "A")
         ;; Set order of multiple groups at once
         (:order-multi (2 (:name "Shopping in town"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :tag "@town"))
                          (:name "Food-related"
                                 ;; Multiple args given in list with implicit OR
                                 :tag ("food" "dinner"))
                          (:name "Personal"
                                 :habit t
                                 :tag "personal")
                          (:name "Space-related (non-moon-or-planet-related)"
                                 ;; Regexps match case-insensitively on the entire entry
                                 :and (:regexp ("space" "NASA")
                                               ;; Boolean NOT also has implicit OR between selectors
                                               :not (:regexp "moon" :tag "planet")))))
         ;; Groups supply their own section names when none are given
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)
         (:priority<= "B"
                      ;; Show this section after "Today" and "Important", because
                      ;; their order is unspecified, defaulting to 0. Sections
                      ;; are displayed lowest-number-first.
                      :order 1)
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         )))
  (org-agenda nil "a"))

;; (use-package org-gcal
;;   :ensure t
;;   :config
;;   (setq org-gcal-client-id (exec-path-from-shell-copy-env "WORK_GMAIL_CAL_CLIENT_ID")
;; 	org-gcal-client-secret (exec-path-from-shell-copy-env "WORK_GMAIL_CAL_CLIENT_SECRET")
;; 	org-gcal-file-alist '(("eric@kamana.com" .  "~/Dropbox/orgfiles/gcal.org"))))
;; 
;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-fetch) ))
;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-fetch)))
