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
      '((height . 27)
        (width . 80)
        (foreground-color . "gray97")
        (background-color . "#2b2b2b")))

(require 'package)
(setq package-enable-at-startup nil)
(dolist (archive '(("melpa-stable" . "http://stable.melpa.org/packages/")
                   ("melpa" . "http://melpa.org/packages/")))
  (add-to-list 'package-archives archive :append))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :preface
  (defun on-a-mac-p ()
    (when (memq window-system '(mac ns))
      t))
  :if (on-a-mac-p)
  :init
  (exec-path-from-shell-initialize))

(use-package emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . " ⨄")
  :config
  (global-undo-tree-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :pin melpa-stable
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
  (setq cider-repl-wrap-history t
	cider-repl-history-size 1000
	cider-repl-history-file "~/.cider-repl-history"
	cider-repl-use-clojure-font-lock t
	cider-repl-display-help-banner nil))

(use-package git-gutter
  :ensure t
  :diminish (git-gutter-mode . " Г")
  :config
  (global-git-gutter-mode +1))

(use-package paredit
  :ensure t
  :pin melpa-stable
  :diminish (paredit-mode . " ⸦⸧")
  :config
  (defvar list-o-hooks
    '(lisp-mode-hook
      emacs-lisp-mode-hook
      ielm-mode-hook
      cider-repl-mode-hook
      clojure-mode-hook))

  (defun add-hooks-to-mode (mode hooks)
    "add a bunch of hooks in one go"
      (dolist (hook hooks)
	(add-hook hook mode)))

  (add-hooks-to-mode #'paredit-mode list-o-hooks))

(use-package git-link
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode "m\\(ark\\)?d\\(own\\)?")

(use-package sort-words
  :ensure t)

(use-package ag
  :ensure t)

(use-package gist
  :ensure t)

(use-package dired+
  :ensure t)

(defun apply-to-mode (value modes)
  (dolist (mode modes)
    (funcall mode value)))

(defun turn-on (modes)
  (apply-to-mode t modes))

(defun turn-off (modes)
  (apply-to-mode -1 modes))

(when window-system
  ;; trim the finges
  (set-fringe-mode '(1 . 1))
  (turn-off '(tool-bar-mode
	      scroll-bar-mode)))

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

;; prevent `custom-set-variables' & `custom-set-faces' from poluting
;; this file.
;; taken from here: http://irreal.org/blog/?p=3765#comment-1896551541
(setq custom-file (make-temp-file "emacs-custom"))
(load custom-file 'noerror)
