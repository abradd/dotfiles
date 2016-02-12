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

;;set default font size
(set-face-attribute 'default nil :height 140)

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

;;org-id
(require 'org-id)

;;set keywords for todo list
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|"  "DONE(@)" "WONT-BE-DONE(@)")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(org-agenda-files
   (quote
    ("~/Dropbox/tasks/quals_prep.org" "~/Google Drive/hydrogel_paper_140830/notes.org" "~/Dropbox/tasks/todo.org")))
 '(org-id-link-to-org-use-id t)
 '(reftex-cite-punctuation (quote (", " " and " " et al"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "white" :foreground "gray100" :inverse-video t)))))

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
	    (?a . (concat "[[papers:%1a/%y_%t][%t]]:" (replace-regexp-in-string "\[\({|}\)\]" "" "%t")))
	    (?j . "[[papers:%1a/%y_%t][%l]]: %t")
	    (?t . "%t")
	    (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))
 

(setq org-link-abbrev-alist
      '(("papers" . "/Users/links_world/Google Drive/Literature/%s.pdf")))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq org-link-abbrev-alist
      '(("papers" . "~/Google Drive/Literature/%s.pdf"))) 

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t)))))

(require 'package)
  (push '("marmalade" . "http://marmalade-repo.org/packages/")
        package-archives )
  (push '("melpa" . "http://melpa.milkbox.net/packages/")
        package-archives)
  (package-initialize)

;;Evil mode

;;evil-leader
(require 'evil-leader)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "x" 'execute-extended-command)

(require 'evil)
(evil-mode 1)

;;(eval-after-load "evil-commands"
 ;; (define-key evil-forward-char "<SPC>" nil))


;;These commands need to be called BEFORE load-theme
;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)

;;Solarized theme

(load-theme 'solarized-dark)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;Stole escape method and jk/kj from [[http://blog.lukeswart.net/wordpress/?p=45][this blog]]

;;; esc quits pretty much anything (like pending prompts in the minibuffer)
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Enable smash escape (ie 'jk' and 'kj' quickly to exit insert mode)
(define-key evil-insert-state-map "k" #'cofi/maybe-exit-kj)
(evil-define-command cofi/maybe-exit-kj ()
:repeat change
(interactive)
(let ((modified (buffer-modified-p)))
(insert "k")
(let ((evt (read-event (format "Insert %c to exit insert state" ?j)
nil 0.5)))
(cond
((null evt) (message ""))
((and (integerp evt) (char-equal evt ?j))
(delete-char -1)
(set-buffer-modified-p modified)
(push 'escape unread-command-events))
(t (setq unread-command-events (append unread-command-events
(list evt))))))))

(define-key evil-insert-state-map "j" #'cofi/maybe-exit-jk)
(evil-define-command cofi/maybe-exit-jk ()
:repeat change
(interactive)
(let ((modified (buffer-modified-p)))
(insert "j")
(let ((evt (read-event (format "Insert %c to exit insert state" ?k)
nil 0.5)))
(cond
((null evt) (message ""))
((and (integerp evt) (char-equal evt ?k))
(delete-char -1)
(set-buffer-modified-p modified)
(push 'escape unread-command-events))
(t (setq unread-command-events (append unread-command-events
(list evt))))))))

(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pptx\\'" "powerpoint" (file))))

;;
;;Latex inline setup
;;
(setq org-latex-create-formula-image-program 'dvipng)

;;Altering the path variable so that latex can be found
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2014/bin/x86_64-darwin/"))
(setq exec-path (append exec-path '("/usr/local/texlive/2014/bin/x86_64-darwin/")))

;;Setting the size of inline latex formulae
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(setq org-capture-templates
      '(("t" "Notes" entry (file+datetree "~/cloud/notes/notebook.org" "Tasks")
            "* %^{Description} %^g %? 
Added: %U" )))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;;Resizing inline images
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (unless refresh
    (org-remove-inline-images)
    (if (fboundp 'clear-image-cache) (clear-image-cache)))
  (save-excursion
    (save-restriction
      (widen)
      (setq beg (or beg (point-min)) end (or end (point-max)))
      (goto-char beg)
      (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
            (substring (org-image-file-name-regexp) 0 -2)
            "\\)\\]" (if include-linked "" "\\]")))
        old file ov img)
    (while (re-search-forward re end t)
      (setq old (get-char-property-and-overlay (match-beginning 1)
                           'org-image-overlay))
      (setq file (expand-file-name
              (concat (or (match-string 3) "") (match-string 4))))
      (when (file-exists-p file)
            (let ((file-thumb (format "%s%st.png" (file-name-directory file) (file-name-base file) "t.png")))
              (unless (file-exists-p file-thumb)
                (shell-command (format "convert %s -thumbnail 600x600 %s"
                                       file file-thumb)))
        (if (and (car-safe old) refresh)
            (image-refresh (overlay-get (cdr old) 'display))
          (setq img (save-match-data (create-image file-thumb)))
          (when img
        (setq ov (make-overlay (match-beginning 0) (match-end 0)))
        (overlay-put ov 'display img)
        (overlay-put ov 'face 'default)
        (overlay-put ov 'org-image-overlay t)
        (overlay-put ov 'modification-hooks
                 (list 'org-display-inline-remove-overlay))
        (push ov org-inline-image-overlays))))))))))

;;mogrify -resize 80x80 -background white -gravity center -extent 80x80 -format jpg -quality 75 -path ../thumbs .

;;global line numbers
(global-linum-mode 1)
