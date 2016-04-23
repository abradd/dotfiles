;; -*- mode: elisp -*-

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;;Turn on git.el
;;(require 'git)

;;Log comletion time
(setq org-log-done 'time)

;;set default font size
(set-face-attribute 'default nil :height 140)

;;Setting up tags
(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

;;Setting indent on startup
(require 'org-indent)
(setq org-startup-indented t)

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

;;Set org-directory
(setq org-directory "~/Dropbox/tasks")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-agenda-files (quote ("~/Dropbox/tasks/todo.org")))
(setq org-mobile-inbox-for-pull "~/Dropbox/inbox.org")

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
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(ledger-reports
   (quote
    (("ByMonthRegChecking" "ledger -f ~/cloud/tasks/fin-data.ledger -M reg checking")
     ("MonthlyReg" "ledger -b \"last month\" reg checking")
     ("balance" "ledger bal")
     ("accounts" "ledger Checking")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(org-agenda-files
   (quote
    ("~/cloud/notes/notebook.org" "~/Dropbox/tasks/quals_prep.org" "~/Google Drive/hydrogel_paper_140830/notes.org" "~/Dropbox/tasks/todo.org")))
 '(org-babel-load-languages (quote ((sh . t) (python . t) (emacs-lisp . t))))
 '(org-capture-templates
   (quote
    (("t" "Notes" entry
      (file+datetree "~/cloud/notes/notebook.org" "Tasks")
      "* %^{Description} %^g %? 
Added: %U")
     ("e" "Emacs info" entry
      (file+datetree "~/cloud/notes/emacsfu.org" "Tasks")
      "* %^{Description} %^g %? 
Added: %U"))))
 '(org-id-link-to-org-use-id t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(reftex-cite-punctuation (quote (", " " and " " et al")))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "white" :foreground "gray100" :inverse-video t)))))


;;org-habit
(require 'org-habit)

(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)
   ;; org-habit-graph-column 80
(setq org-insert-heading-respect-content nil)

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
      '(("papers" . "/Users/links_world/Google Drive/literature/%s.pdf")))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(setq org-link-abbrev-alist
      '(("papers" . "~/Google Drive/literature/%s.pdf"))) 

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t)))))

;;Evil mode

;;evil-leader
(require 'evil-leader)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "x" 'execute-extended-command)
(evil-leader/set-key "ee" 'eval-buffer)

(require 'evil)
(evil-mode 1)

;;acejump enable
(evil-leader/set-key "fw" 'evil-ace-jump-word-mode) ; ,e for Ace Jump (word)
(evil-leader/set-key "fl" 'evil-ace-jump-line-mode) ; ,l for Ace Jump (line)
(evil-leader/set-key "ff" 'evil-ace-jump-char-mode) ; ,f for Ace Jump (char)

(defun open-notebook()
  "Open lab notebook"
  (interactive)
  (find-file "~/cloud/notes/notebook.org")
  )
(defun open-ledger()
  "Open ledger file"
  (interactive)
  (find-file "~/cloud/tasks/fin-data.ledger")
  )
(defun open-emacs()
  "Open .emacs file"
  (interactive)
  (find-file "~/dotfiles/.emacs")
  )
(defun open-todo()
  "Open todo file"
  (interactive)
  (find-file "~/cloud/tasks/todo.org")
  )

;ease of use shortcuts
(evil-leader/set-key "on" 'open-notebook);
(evil-leader/set-key "ol" 'open-ledger) ;
(evil-leader/set-key "oe" 'open-emacs) ;
(evil-leader/set-key "ot" 'open-todo) ;

;;evil-nerd-commenter [[https://github.com/redguardtoo/evil-nerd-commenter][link]]
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
)

;;flyspell, mostly from https://www.emacswiki.org/emacs/FlySpell#toc7
(dolist (hook '(org-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )

(evil-leader/set-key
  "zz" 'ispell-word
  "zn" 'flyspell-check-next-highlighted-word)

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

;;indenting
(setq-default tab-width 4 indent-tabs-mode nil)

;;no jumpback when exiting insert mode
(setq evil-move-cursor-back nil)

;;smooth-scrolling http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
(setq scroll-margin 5
scroll-conservatively 9999
scroll-step 1)

;;powerline-evil setup

;; (require 'powerline)
;; (powerline-default-theme)
;; (powerline-evil-vim-color-theme)
;; (display-time-mode t)

;;page-up and page-down
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))

;;Solarized theme

(load-theme 'solarized-dark)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;stolen from http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
;; (defun minibuffer-keyboard-quit ()
;;   "Abort recursive edit.
;; In Delete Selection mode, if the mark is active, just deactivate it;
;; then it takes a second \\[keyboard-quit] to abort the minibuffer."
;;   (interactive)
;;   (if (and delete-selection-mode transient-mark-mode mark-active)
;;       (setq deactivate-mark  t)
;;     (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;;     (abort-recursive-edit))

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


;;PATH additions
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/links_world/src")))
(setq load-path (append load-path '("/usr/local/bin")))

(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pptx\\'" "powerpoint" (file))))
;; (setq openwith-associations '(("\\.JPG\\'" "open" (file))))

;;
;;Latex inline setup
;;
(setq org-latex-create-formula-image-program 'dvipng)

;;Altering the path variable so that latex can be found
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2014/bin/x86_64-darwin/"))
(setq exec-path (append exec-path '("/usr/local/texlive/2014/bin/x86_64-darwin/")))

;;Setting the size of inline latex formulae
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))


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
            (let ((file-thumb (format "%s%st.jpg" (file-name-directory file) (file-name-base file) "t.jpg")))
              (unless (file-exists-p file-thumb)
                (shell-command (format "convert %s -thumbnail 500x %s"
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
;; (global-linum-mode 1)
(global-nlinum-mode 1)

;ido-mode
(require `ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;org-reveal
(require 'ox-reveal)

(setq org-reveal-root "file:///Users/links_world/reveal.js")

;;ledger
(autoload 'ledger-mode "ledger-mode" "A major mode for Ledger" t)
(add-to-list 'load-path
             (expand-file-name "/usr/local/Cellar/ledger/3.1.1_1/share/emacs/site-lisp/ledger/"))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(setq ledger-binary-path "/usr/local/bin/ledger")

(evil-leader/set-key "lc" 'ledger-mode-clean-buffer)

;;magit setup
(require 'magit)

;;org-ref setup
(setq reftex-default-bibliography '("~/Google Drive/literature/library.bib"))

(require 'org-ref)
;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Google Drive/literature/notes.org"
      org-ref-default-bibliography '("~/Google Drive/literature/library.bib")
      org-ref-pdf-directory "~/Dropbox/test/"
      org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)

;;helm-bibtex
(setq helm-bibtex-pdf-field "File")
(setq helm-bibtex-bibliography "~/Google Drive/literature/library.bib")
;(setq helm-bibtex-library-path "~/Dropbox/bibliography/bibtex-pdfs")

;; open pdf with system pdf viewer (works on mac)
;(setq helm-bibtex-pdf-open-function
  ;(lambda (fpath)
    ;(start-process "open" "*open*" "open" fpath)))
(setq helm-bibtex-pdf-open-function
  (lambda (fpath)
    (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)))

;; alternative
;;(setq helm-bibtex-pdf-open-function 'org-open-file)

(setq helm-bibtex-notes-path "~/Dropbox/bibliography/helm-bibtex-notes")

;calc tricks
(defun calc-eval-region (arg beg end)
  "Calculate the region and display the result in the echo area.
With prefix ARG non-nil, insert the result at the end of region."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (if (null arg)
        (message "%s = %s" expr result)
      (goto-char end)
      (save-excursion
        (insert (concat "=" result))))))


;;smartparens
(require 'smartparens-config)

(smartparens-mode 1)

;;org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;header changes
;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))


