;; -*- mode: elisp -*-

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;;Set org-directory
(setq org-directory "~/Dropbox/tasks")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-agenda-files (quote ("~/Dropbox/tasks/todo.org")))
(setq org-mobile-inbox-for-pull "~/Dropbox/inbox.org")
			      
;;Turn on git.el
;;(require 'git)

;;Log comletion time
(setq org-log-done 'time)

;;Setting up tags
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

;;Setting indent on startup
(setq org-startup-indented 'indent)

;;Soft wrapping at window edge
(global-visual-line-mode 1)

;; Enable transient mark mode
(transient-mark-mode 1)

;;Set shorcut for agenda mode
(define-key global-map "\C-ca" 'org-agenda)

;;;;org-mode configuration
;; Enable org-mode
(require 'org)
;; Make org-mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

;;set keywords for todo list
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|"  "DONE(@)" "WONT-BE-DONE(@)")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Google Drive/hydrogel_paper_140830/notes.org" "~/Dropbox/tasks/todo.org")))
 '(reftex-cite-punctuation (quote (", " " and " " et al"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;Setting up RefTeX

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;enable auto-revert-mode to update reftex when bibtex file changes on disk
	 (global-auto-revert-mode t)
	 (reftex-parse-all)
	 ;add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?b . "[[bib:%l][%l-bib]]")
	    (?n . "[[notes:%l][%l-notes]]")
	    (?p . "[[papers:%l][%l-paper]]")
	    (?A . "[[papers:%A][%A-paper]]")
	    (?a . "[[papers:%1a/%y_%t][%l]]: %t")
	    (?t . "%t")
	    (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))
 
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t)))))


