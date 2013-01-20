;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   A major mode for editing Blink schemas
;;
;;   Version: 1.0
;;   Date: 2012-12-03
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Copyright (c) 2012, Pantor Engineering AB
;;   All rights reserved.
;;   
;;   Redistribution and use in source and binary forms, with or
;;   without modification, are permitted provided that the following
;;   conditions are met:
;;   
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer. 
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials provided
;;     with the distribution.
;;
;;   * Neither the name of Pantor Engineering AB nor the names of its
;;     contributors may be used to endorse or promote products derived
;;     from this software without specific prior written permission.
;;   
;;   THIS   SOFTWARE   IS PROVIDED BY    THE   COPYRIGHT  HOLDERS  AND
;;   CONTRIBUTORS "AS IS"   AND ANY  EXPRESS OR  IMPLIED   WARRANTIES,
;;   INCLUDING,  BUT  NOT LIMITED  TO,   THE  IMPLIED  WARRANTIES   OF
;;   MERCHANTABILITY    AND  FITNESS  FOR   A  PARTICULAR  PURPOSE ARE
;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;   BE  LIABLE   FOR ANY    DIRECT, INDIRECT,   INCIDENTAL,  SPECIAL,
;;   EXEMPLARY, OR CONSEQUENTIAL DAMAGES  (INCLUDING, BUT NOT  LIMITED
;;   TO, PROCUREMENT  OF  SUBSTITUTE GOODS OR  SERVICES;  LOSS OF USE,
;;   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;   ANY THEORY OF  LIABILITY, WHETHER IN CONTRACT,  STRICT LIABILITY,
;;   OR  TORT (INCLUDING NEGLIGENCE OR  OTHERWISE) ARISING  IN ANY WAY
;;   OUT OF  THE  USE OF   THIS  SOFTWARE,  EVEN IF ADVISED   OF   THE
;;   POSSIBILITY OF SUCH DAMAGE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Example setup for your ~/.emacs file:
;;
;;   (autoload 'blink-mode "blink-mode")
;;   (setq auto-mode-alist       
;;         (cons '("\\.blink\\'" . blink-mode) auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

(defvar blink-indent-level 2 "The Blink indentation level.")

(defvar blink-decls
  (mapcar (lambda (kw) (concat "\\b\\(?:" kw "\\)\\b"))
	  '("namespace"))
  "Blink special forms")

(defvar blink-types
  (mapcar (lambda (kw) (concat "\\b%\\s *\\(?:" kw "\\)\\b"))
	  '("i8" "u8" "i16" "u16" "i32" "u32" "i64" "u64"  
	    "f64" "decimal" "date" "timeOfDay" "nanotime" "millitime" 
	    "bool" "string" "object"))
  "Blink types")

(defvar blink-keywords
  (mapcar (lambda (kw) (concat "\\b\\(?:" kw "\\)\\b"))
	  '("type"))
  "Blink keywords")

(defun blink-make-regexp-choice (operands)
  "(op1 op2 ...) -> \"\\(op1\\|op2\\|...\\)\""
  (let ((result "\\("))
    (mapcar (lambda (op) (setq result (concat result op "\\|"))) operands)
    (concat (substring result 0 -2) "\\)")))

(defconst blink-light-blue-color "#9292C9")
(defconst blink-dark-blue-color "#3A3A7B")
(defconst blink-green-color "#257A25")
(defconst blink-grey-color "#666666")

(defconst blink-sky-blue-color "#ACACFC")
(defconst blink-dark-green-color "#00AD00")
(defconst blink-light-green-color "#70F170")

(defgroup blink-highlighting-faces nil
  "Faces for Blink syntax highlighting."
  :group 'font-lock-highlighting-faces)

(defface blink-annot-face
  `((((class color) (background light)) 
     (:foreground ,blink-green-color :italic t))
    (((class color) (background dark :italic t)) 
     (:foreground ,blink-light-green-color)))
  "Face used to highlight annotations."
  :group 'blink-highlighting-faces)

(defface blink-type-face
  `((((class color) (background light)) (:foreground ,blink-grey-color))
    (((class color) (background dark)) (:foreground ,blink-sky-blue-color)))
  "Face used to highlight references."
  :group 'blink-highlighting-faces)

;; Font lock treats face names differently in GNU Emacs and XEmacs
;; The following defvars is a workaround

(defvar italic 'italic)
(defvar default 'default)
(defvar font-lock-preprocessor-face 'font-lock-preprocessor-face)
(defvar blink-annot-face 'blink-annot-face)
(defvar blink-type-face 'blink-type-face)

(defvar blink-font-lock-keywords
  (list
   (list 
    (concat (blink-make-regexp-choice blink-decls) "\\s *\\([^ \t\n]+\\)")
    '(1 font-lock-keyword-face) '(2 font-lock-constant-face))

   (list (blink-make-regexp-choice blink-keywords) 1 font-lock-keyword-face)

   '("\\(?:\\s +\\|,\\|=\\)\\([a-zA-Z_][a-zA-Z0-9_]*\\(?::[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)\\(?:\\s *?\\*\\)?\\(?:\\s *\\[\\s *\\]\\)?\\(?:\\s \\|\n\\)+[a-zA-Z_][a-zA-Z0-9_]*"
     1 blink-type-face)

   '("^[a-zA-Z_][a-zA-Z0-9_]*" 0 font-lock-function-name-face)

   '("^\\([a-zA-Z_][a-zA-Z0-9_:]*\\)\\(?:\\s \\|\n\\)*<-" 1 blink-type-face t)

   '(":\\(?:\\s \\|\n\\)+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 blink-type-face)

   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\):\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 
     1 font-lock-constant-face t)

   '("\\(string\\)\\s *\\(([^)]+)\\)"
     (1 blink-type-face)
     (2 font-lock-string-face))
   
   '("\\(@[a-zA-Z_][a-zA-Z0-9_:]*\\)\\(?:\\s \\|\n\\)*=\\(?:\\s \\|\n\\)*"
     1 blink-annot-face t)

   '("|\\(?:\\s \\|\n\\)*\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     1 font-lock-string-face t)
   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(?:\\s \\|\n\\)*|"
     1 font-lock-string-face t)
   )
  "Blink Highlighting")


(defun blink-find-column (first start is-incr-annot)
  "Find which column to indent to." 

  ;; FIXME: backward-sexp doesn't work with unbalanced braces in comments

  (let* (column
	 pos
	 ;; Find start of enclosing block or assignment
	 (token
	  (catch 'done
	    (while (setq pos (re-search-backward "=\\|->\\|<-" (point-min) t))
	      (let ((c (match-string 0)))
		(beginning-of-line)
		(re-search-forward "\\S ")
		(setq column (- (current-column) 1))
		(beginning-of-line)
		(cond
		 ;; Don't match inside comments or annots
		 ;; FIXME: Should exclude matches inside string literals too
		 ((re-search-forward "#" pos t) (beginning-of-line))
		 ((string= c "<-") (throw 'done 'annot))
		 ((re-search-forward "@" pos t) (beginning-of-line))
		 ;; Skip block
		 ((string= c "=") (throw 'done 'def))
		 ((string= c "->") (throw 'done 'def))
		 (t (throw 'done 'annot))))))))

    (cond
     ((not pos) 0)

     ;; Check if first preceding non-whitespace character was an operator
     ;; If not, this is most likely a top level clause.

     ((or (eq token 'def))
      (goto-char start)
      (if (and (setq pos (find-prev-grammar-char))
	       (or
		(member (match-string 0) '("," "=" ":" "|"))
		(string= (buffer-substring (- pos 1) (+ pos 1)) "<-")
		(string= (buffer-substring (- pos 1) (+ pos 1)) "->")))
	  (+ column blink-indent-level)
	column))
     (is-incr-annot
      (goto-char pos)
      (beginning-of-line)
      (search-forward "<-" (+ pos 2) t)
      (- (current-column) 2))
     (t 0))))

(defun find-prev-grammar-char ()
  "Finds the first non ws char that is not part of a comment"
  (save-excursion
    (let ((pos -1) start)
      (while (< pos 0)
	(setq pos (re-search-backward "[^ \t\n]" (point-min) t))
	(cond ((not pos) (setq pos (point-min)))
	      ((setq start (search-backward "#" (find-start-of-line pos) t))
	       (setq pos -1)
	       (goto-char start))))
      pos)))

(defun find-start-of-line (pos)
  "Finds a start of line before pos"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (point)))

(defun get-end-of-line ()
  "Finds the end of the current line"
  (save-excursion (end-of-line) (point)))

(defun blink-indent-line ()
  "Indents the current line."
  (interactive)
  (let ((orig-point (point)))
    (beginning-of-line)

    (let* ((is-incr-annot 
	    (save-excursion (re-search-forward "^\\s *<-" (get-end-of-line) t)))
	   (beg-of-line (point))
	   (pos (re-search-forward "\\(\\S \\|\n\\)" (point-max) t))
	   (first (match-string 0))
	   (start (match-beginning 0))
	   (col (- (current-column) 1)))

      (goto-char beg-of-line)

      (let ((indent-column (blink-find-column first start is-incr-annot)))
	(goto-char beg-of-line)

	(cond
	 ;; Only modify buffer if the line must be reindented
	 ((not (= col indent-column))
	  (if (not (or (null pos)
		       (= beg-of-line start)))
	      (kill-region beg-of-line start))

	  (goto-char beg-of-line)
	  
	  (while (< 0 indent-column)
	    (insert " ")
	    (setq indent-column (- indent-column 1))))

	 ((< orig-point start) (goto-char start))
	 (t (goto-char orig-point)))))))


(defun blink-electric-brace (arg)
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (blink-indent-line)
  (let ((p (point)))
    (when (save-excursion
	    (beginning-of-line)
	    (let ((pos (re-search-forward "\\S " (point-max) t)))
	      (and pos (= (- pos 1) p))))
      (forward-char-command))))

(defvar blink-mode-map () "Keymap used in Blink mode.")
(when (not blink-mode-map)
  (setq blink-mode-map (make-sparse-keymap))
  (define-key blink-mode-map "\C-c\C-c" 'comment-region)
  (define-key blink-mode-map "]" 'blink-electric-brace)
  (define-key blink-mode-map "[" 'blink-electric-brace))

(defun blink-mode ()
  "Major mode for editing Extensible Contact Notation (Blink) documents.
\\{blink-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'blink-indent-line)
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(blink-font-lock-keywords nil nil nil nil))
  
  (use-local-map blink-mode-map)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  
  (setq comment-start "#"
	comment-end ""
	comment-start-skip "\\([ \n\t]+\\)##?[ \n\t]+")
  
  (let ((blink-syntax-table (copy-syntax-table)))
    (modify-syntax-entry ?# "<   " blink-syntax-table)
    (modify-syntax-entry ?\n ">   " blink-syntax-table)
    (modify-syntax-entry ?\^m ">   " blink-syntax-table)
    (modify-syntax-entry ?\\ "w   " blink-syntax-table)
    (modify-syntax-entry ?' "\"   " blink-syntax-table)
    (modify-syntax-entry ?. "w   " blink-syntax-table)
    (modify-syntax-entry ?- "w   " blink-syntax-table)
    (modify-syntax-entry ?_ "w   " blink-syntax-table)
    (set-syntax-table blink-syntax-table))
  
  (setq mode-name "Blink"
	major-mode 'blink-mode)
  (run-hooks 'blink-mode-hook))

(provide 'blink-mode)
