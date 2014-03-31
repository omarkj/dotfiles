(require 'cl)

;; Config
(setq inhibit-startup-message t
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      org-remember-default-headline 'bottom
      org-completion-use-ido t
      visible-bell t
      vc-follow-symlinks t
      custom-packages-root (getenv "EMACS_UNMANAGEDPACKAGESROOT"))

;; Packages
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(better-defaults clojure-mode idle-highlight-mode
                                      find-file-in-project magit go-mode
                                      markdown-mode yaml-mode page-break-lines
                                      json-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Erlang
(defun run-erl (erl-cmd)
  (let* ((cmd (format "erl -eval 'erlang:display(%s), halt().'  -noshell" erl-cmd))
         (result1 (shell-command-to-string cmd))
         (result2 (replace-regexp-in-string "\r" ""  result1))
         (result (replace-regexp-in-string "\"" "" result2)))
    (substring result 0 -1)))

(defun setup-erl ()
  (let* ((erl-path (run-erl "code:root_dir()"))
         (erl-tools (run-erl "code:lib_dir(tools, emacs)"))
         (erl-exec-path (format "%s/bin" erl-path)))
    (setq load-path (cons erl-tools load-path)
          erlang-root-dir erl-path
          exec-path (cons erl-exec-path exec-path)))
  (require 'erlang-start)
  ; Initiate edts if the folder exists
  (let ((edts-folder (format "%s/edts" custom-packages-root)))
    (when (file-directory-p edts-folder)
      (setq load-path (cons edts-folder load-path))
      (require 'edts-start))))
(setup-erl)


