(setq inhibit-splash-screen t)
(set-face-attribute 'default nil :height 160)
(set-background-color "#2b2b2b")
(set-foreground-color "gray97")
(set-face-background 'mode-line "orange1")
(set-face-foreground 'mode-line "black")

;; the colours above are set so that it loads them quick on start up
;; below are options that every subsequent frame will obtain
;; NOTE:
;; just having the 'default-frame-alist' set without the colour
;; choices above is a bit slow so Emacs starts with the default white
;; background for a split second
(setq default-frame-alist
      '((foreground-color . "gray97")
        (background-color . "#2b2b2b")))

  (defun add-hooks-to-mode (mode hooks)
    "add a bunch of `hooks' to a `mode', in one go"
      (dolist (hook hooks)
	(add-hook hook mode)))

(require 'package)
(setq package-enable-at-startup nil)
(dolist (archive '(("melpa-stable" . "http://stable.melpa.org/packages/")
                   ("melpa" . "http://melpa.org/packages/")))
  (add-to-list 'package-archives archive :append))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defun mac-p ()
  "Return t if the current system is OSX/macOS"
  (eq system-type 'darwin))

(when (mac-p)
  (progn
    (condition-case nil
	(progn
	  (set-default-font "Fira Code")
	  (set-face-attribute 'default nil :height 180))
      (error
       (progn
	 (set-default-font "Monaco")
	 (set-face-attribute 'default nil :height 160))))
    (mac-auto-operator-composition-mode)))

(use-package exec-path-from-shell
  :ensure t
  :if (mac-p)
  :init
  (exec-path-from-shell-initialize))

(use-package dedicated
  :ensure t
  :diminish (dedicated-mode . " །"))

(use-package subword
  :init (global-subword-mode)
  :diminish subword-mode)

(use-package clojure-mode
  :ensure t
  :defer t
  :init

  (use-package fold-this
    :ensure t)

  (use-package clojure-snippets
    :ensure t
    :config
    (add-hook 'clojure-mode-hook 'yas-minor-mode)
    (setq yas-also-auto-indent-first-line t))

  (use-package clojars
    :ensure t)

  (use-package clj-refactor
    :pin melpa-stable
    :ensure t
    :config
    (setq cljr-favor-prefix-notation nil)
    (add-hook 'clojure-mode-hook
	      (lambda () (clj-refactor-mode 1))
	      '(cljr-add-keybindings-with-prefix "C-c R"))))

(use-package company
  :ensure t
  :defer t
  :config (global-company-mode))

(use-package cider
  :ensure t
  :pin melpa-stable
  :diminish (cider-mode . " ￠")
  :commands (cider jack-in cider-jack-in cider-connect)
  :bind ("C-x p" . get-project-file)
  :init
  (defun get-project-file ()
    "open project.clj
     expects the project to be under git, as it uses magit."
    (interactive)
    (if (featurep 'magit)
	(find-file-other-window (concat (magit-toplevel) "/project.clj"))
      (message "install magit for this to work")))

  (defun get-ns ()
    "copy the name of the namespace of the Clojure file"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (forward-word)
      (forward-char)
      (kill-ring-save (point) (line-end-position))))

  :config
  (defalias 'jack-in 'cider-jack-in)
  (defalias 'enlighten 'cider-enlighten-mode)
  (setq nrepl-log-messages t
	cider-repl-wrap-history t
	cider-repl-history-size 1000
	cider-repl-history-file "~/.cider-repl-history"
	cider-repl-use-clojure-font-lock t
	cider-repl-display-help-banner nil
	cider-repl-use-pretty-printing t
	cider-prompt-save-file-on-load 'always-save)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

(use-package html-to-hiccup
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . " ⨄")
  :config
  (global-undo-tree-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :ensure t
  :diminish (git-gutter-mode . " Г")
  :config
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :ensure t)

(use-package smartparens
  :ensure t
  :diminish (smartparens-mode . " ⸦⸧")
  :diminish (smartparens-strict-mode . "/ᔑ")
  :init
  (use-package smartparens-config)
  (smartparens-global-mode 1)
  :config
  (sp-use-paredit-bindings)
  (add-hook 'smartparens-enabled-hook #'smartparens-strict-mode)
  (add-hook 'smartparens-mode #'scss-mode))

(use-package git-link
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	  (".m\\(ark\\)?d\\(own\\)?\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package sort-words
  :ensure t)

(use-package ag
  :ensure t)

(use-package gist
  :ensure t)

(use-package dired+
  :ensure t
  :init
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))

(use-package sql-mode
  :commands sql-mode)

(use-package sql-interactive-mode
  ;; :commands sql-interactive-mode
  :defer t
  :init
  ;; PostgreSQL databases with underscores in their names trip up the
  ;; prompt specified in sql.el. I work around this with the
  ;; following. Warning, this sets the prompt globally, which is fine
  ;; by me since I only ever use Postgres. - Luke Burton
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
              (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))

  (defun my-sql-save-history-hook ()
    "Save SQL query history.
Taken from: https://www.emacswiki.org/emacs/SqlMode"
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
          (let ((filename
                 (concat "~/.emacs.d/sql/"
                         (symbol-name (symbol-value rval))
                         "-history.sql")))
            (set (make-local-variable lval) filename))
        (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))

  (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook))


(use-package sqlup-mode
  :ensure t
  :diminish (sqlup-mode . " ⧌")
  :config
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(use-package css-mode
  :config
  (add-hook 'scss-mode-hook
	    (lambda()
	      (setq css-indent-offset 2
		    tab-width 2
		    scss-compile-at-save nil))))

(use-package sass-mode
  :ensure t)

(use-package flycheck-package
  :ensure t)

(use-package dash-at-point
  :ensure t
  :bind (("C-c d" . dash-at-point)
	 ("C-c e" . dash-at-point-with-docset)))

(use-package docker
  :ensure t
  :diminish (docker-mode . " ᗪ"))

(use-package dockerfile-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :config
  (defalias 'json-pretty-print 'json-mode-beautify))

(use-package projectile
  :ensure t)

(defun apply-to-mode (value modes)
  (dolist (mode modes)
    (funcall mode value)))

(defun turn-on (modes)
  (apply-to-mode t modes))

(defun turn-off (modes)
  (apply-to-mode -1 modes))

(when window-system
  (set-fringe-mode '(1 . 1)) ;; trim the finges
  (turn-off '(tool-bar-mode
	      scroll-bar-mode
	      blink-cursor-mode)))

(turn-on '(show-paren-mode))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (when window-system
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(when (memq window-system '(mac ns))
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)
  (global-set-key [?£] '(lambda ()
                          (interactive)
                          (insert "#")))
  (global-set-key [f8] 'toggle-fullscreen))

;; shorter yes/no
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(use-package ido
  :init (ido-mode t)
  :config (setq ido-enable-flex-matching t))

(use-package woman
  :commands (woman man)
  :config
  (defalias 'man 'woman)
  (setq woman-use-own-frame nil))

(use-package eshell
  :bind ("M-3" . goto-eshell)
  :config
  (use-package eshell-up
    :ensure t)

  (defun goto-eshell()
    "switch to eshell buffer v2"
    (interactive)
    (if (get-buffer "*eshell*")
        (switch-to-buffer-other-window "*eshell*")
      (eshell)))
  :init
  (add-hook 'eshell-mode-hook
	    (lambda ()
              "Add screen clear function & keybinding (C-l) for eshell.
               For some reason adding this function to definition
               to the `:config' and key binding to `:bind' would
               error."
              (defun eshell/clear()
                "original taken from: 04Dec2001 - sailor,
                 added eshell-send-input based on comments by
                 parv at linuxquestions forum"
                (interactive)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (eshell-send-input)))
	      (define-key eshell-mode-map (kbd "C-l") 'eshell/clear))))

(use-package desktop
  :config
  (setq desktop-dirname "~/.emacs.d")
  (desktop-save-mode t))

(use-package nxml-mode
  :config
  (defun xml-pretty-print (begin end)
    "Pretty format XML markup in region. You need to have nxml-mode
    http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
    this.  The function inserts linebreaks to separate tags that have
    nothing but whitespace between them.  It then indents the markup
    by using nxml's indentation rules.

    Original author Benjamin Ferrari:
    http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html"
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!")))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer-other-window))

(use-package org
  :config
  (defun dotemacs-org-src-wrap (start end)
    "Prompt the user for a language, then wrap it in org-mode
source block"
    (interactive "r")
    (let ((lang (read-string "Enter the programming language: ")))
      (save-excursion
	(goto-char end)
	(insert "\n#+END_SRC\n")
	(goto-char start)
	(insert "#+BEGIN_SRC " lang "\n"))))

  (define-key org-mode-map (kbd "C-<") 'dotemacs-org-src-wrap))

(use-package adoc-mode
  :ensure t
  :defer t)

(defun tmpbuf (buf)
    "open a buffer,
  if it doesn't exist, open a new one"
    (interactive "sBuffer name: ")
    (switch-to-buffer
     (get-buffer-create (concat "*" buf "*"))))

(add-hook 'before-save-hook
	  'delete-trailing-whitespace)

(global-set-key (kbd "M-g") 'goto-line)

;; Save files when the focus is lost.
;; Taken from Bozhidar Batsov:
;; http://emacsredux.com/blog/2014/03/22/a-peek-at-emacs-24-dot-4-focus-hooks/

(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

;; convenience
(add-hook 'find-file-hook
            (lambda()
              (highlight-phrase "\\(BUG\\|FIXME\\|TODO\\|NOTE\\):")))

;; prevent `custom-set-variables' & `custom-set-faces' from poluting
;; this file.
;; taken from here: http://irreal.org/blog/?p=3765#comment-1896551541
(setq custom-file (make-temp-file "emacs-custom"))
(load custom-file 'noerror)

(defun sumup()
    "Work out the sum for a cua-mode selected column/rectangle
  and add it to the kill ring, so that it can be pasted in.
  Improves on: http://www.emacswiki.org/emacs/AddNumbers as it
  works on the more precise, visually appealing & obvious
  cua-rectangle selected area, instead of the 'plain' rectangle
  select."
    (interactive)
    (save-excursion
      (let ((sum 0)
        (sumup "*sumup*"))
        (cua-copy-rectangle-as-text)
        (set-buffer (get-buffer-create sumup))
        (erase-buffer)
        (yank)
        (goto-char (point-min))
        (while (re-search-forward "[0-9]*\\.?[0-9]+" nil t)
          (setq sum (+ sum (string-to-number (match-string 0)))))
        (message "Sum: %f" sum)
        (kill-new (number-to-string sum))
        (kill-buffer sumup))))

(cua-selection-mode t)
(defalias 'block-edit 'cua-set-rectangle-mark)
(global-set-key "\M-[" 'block-edit)


(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(global-set-key (kbd "C-%") 'forward-or-backward-sexp)

(defun dired-dir-in-eshell ()
  "Open the current directory in Eshell"
  (interactive)
  (if (string= major-mode "dired-mode")
      (if (get-buffer "*eshell*")
	  (progn (switch-to-buffer-other-window "*eshell*")
		 (insert (format "%s" dired-directory))
		 (eshell-send-input))
	(eshell))
    (message "This function in only intended to be used in dired-mode")))
