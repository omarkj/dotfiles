(require 'cl)
;; ELPA
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(custom-set-variables
 '(current-language-environment "UTF-8")
 '(ecb-options-version "2.40")
 '(ido-case-fold t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ns-alternate-modifier nil)
 '(ns-right-alternate-modifier (quote meta))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(safe-local-variable-values (quote ((indent-tabs-mode . s))))
 '(visual-line-mode nil t)
)

;; Load files
(let ((base "~/.emacs.d/files"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))
;; Require
(require 'package)
;; Packages
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(loop for p in '(nrepl coffee-mode php-mode
		       puppet-mode markdown-mode
		       js2-mode clojure-mode erlang
		       clojure-project-mode clojurescript-mode
		       color-theme zenburn-theme auto-complete
		       ac-nrepl) do (when (not (package-installed-p p))
								     (package-install p)))
(loop for s in '(ido flymake flymake-cursor
		     color-theme puppet-mode
		     auto-complete-config
		     sr-speedbar package) do (require s))
;; AC
(global-auto-complete-mode t)
;; Themes
(color-theme-zenburn)
;; Flymake colors
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
;; Load Erlang
(setq load-path (cons "~/emacs.d/files/erlang" load-path))
(load "erlang-mode")

;; Iced Coffee
(add-to-list 'auto-mode-alist '("\\.iced$" . coffee-mode))

;; Puppet files
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; JSON
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.8/emacs" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)
(add-hook 'erlang-mode-hook 'my-erlang-hook-function)
(defun my-erlang-hook-function ()
  (imenu-add-to-menubar "Functions"))

;; This is needed for Distel setup
(let ((distel-dir "~/.emacs.d/distel/elisp"))
  (unless (member distel-dir load-path)
    ;; Add distel-dir to the end of load-path
    (setq load-path (append load-path (list distel-dir)))))
(require 'distel)
(distel-setup)

(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-sname" "emacs"))
	    ;; add Erlang functions to an imenu menu
	    (imenu-add-to-menubar "imenu")))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-p"      erl-complete)	
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind) 
    ("\M-+"      erl-find-source-unwind) 
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
