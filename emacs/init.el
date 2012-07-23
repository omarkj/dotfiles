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

(setq load-path (cons "~/emacs.d/files/erlang" load-path))

;; Require
(require 'ido)
(require 'coffee-mode)
(require 'stylus-mode)
(require 'php-mode)
(require 'flymake)
(require 'flymake-cursor)
(require 'color-theme)
(require 'puppet-mode)
(require 'sr-speedbar)

(setq remem-prog-dir "~/.emacs.d/files/remem")
(setq remem-database-dir "~/.emacs.d/files/remem/db")
(setq remem-scopes-list '(("my-email" 6 5 500)
			  ("my-notes" 2 10 500)))
(require 'remem)

;; Load Erlang
(load "erlang-mode")

;; Iced Coffee
(add-to-list 'auto-mode-alist '("\\.iced$" . coffee-mode))

;; Puppet files
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; JSON
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Flymake colors
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Themes
(color-theme-initialize)
(color-theme-zenburn)
