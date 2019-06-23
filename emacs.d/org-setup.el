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
			     "~/Dropbox/orgfiles/birthdays.org"))
; Set key combos
(define-key global-map "\C-ca" 'org-agenda)

(setq org-capture-templates
      '(("n" "Note" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Notes")
	 "* Note %?\n%T")
	("i" "Blog Idea" entry (file+headline "~/Dropbox/orgfiles/blog.org" "Blog Idea")
	 "* Blog %?\n%T")
	("l" "Link" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
	 "* %? %^L %^g \n%T" :prepend t)
	 ("y" "Youtube" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Youtube")
	  "* Note %?\n%T")
	("t" "Life To Do" entry (file+headline "~/Dropbox/orgfiles/life.org" "To Do Items")
	  (file "~/dotfiles/emacs.d/template-todo.txt"))
	("w" "Work To Do" entry (file+headline "~/Dropbox/orgfiles/work.org" "To Do Items")
	  (file "~/dotfiles/emacs.d/template-todo.txt"))
	("m" "Movie" entry (file+headline "~/Dropbox/orgfiles/life.org" "Movies")
	  (file "~/dotfiles/emacs.d/template-movie.txt"))
	("j" "Journal" entry (file+datetree "~/Dropbox/orgfiles/journal.org") 
	"** %^{Title}")
	("g" "Goals") 
	 ("ge" "Epic goals" entry (file+headline "~/Dropbox/orgfiles/goals.org" "Epic Goals") 
	   (file "~/dotfiles/emacs.d/template-goal.txt") :empty-lines-after 1)
	 ("gl" "Long term goal (2-5 years from now)" entry (file+headline "~/Dropbox/orgfiles/goals.org" "Long term goals") 
	   (file "~/dotfiles/emacs.d/template-goal.txt") :empty-lines-after 1) 
	 ("gm" "Medium term goal (6 months up to 2 years)" entry (file+headline "~/Dropbox/orgfiles/goals.org" "Medium term goals") 
	   (file "~/dotfiles/emacs.d/template-goal.txt") :empty-lines-after 1) 
	 ("gs" "Short term goals (next 6 months)" entry (file+headline "~/Dropbox/orgfiles/goals.org" "Short term goals") 
	   (file "~/dotfiles.emacs.d/template-goal.txt") :empty-lines-after 1)))

(setq bookmark-default-file "~/Dropbox/orgfiles/bookmarks.bmk" bookmark-save-flag 1)

(defun make-capture-frame ()
 "Create a new frame and run org-capture."
 (interactive)
 (make-frame '((name . "capture")))
 (select-frame-by-name "capture")
 (delete-other-windows)
 (org-capture))

(setq org-log-done 'time)

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

(defun copy-id-to-clipboard () 
  (interactive)
    (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
    (setq mytmpid (funcall 'org-id-get-create))
    (kill-new mytmpid)
    (message "Copied %s to killring (clipboard)" mytmpid)
  ))
 
(global-set-key (kbd "C-c id") 'copy-id-to-clipboard)
	
(defun org-add-ids-to-headlines-in-file ()
  (interactive)
  (org-map-entries 'org-id-get-create))

;(add-hook 'org-mode-hook
;  (lambda ()
;    (add-hook 'before-save-hook 'org-add-ids-to-headlines-in-file nil 'local)))
