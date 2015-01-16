;;; asm-mode.el --- mode for editing assembler code
;;; Code:

(defgroup kick nil
  "Mode for editing assembler code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom kick-comment-char ?\;
  "The comment-start character assumed by Kick mode."
  :type 'character
  :group 'kick)

(defvar kick-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    st)
  "Syntax table used while in Kick mode.")

(defvar kick-mode-abbrev-table nil
  "Abbrev table used while in Kick mode.")
(define-abbrev-table 'kick-mode-abbrev-table ())

(defvar kick-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Kick mode.")

(defconst kick-font-lock-keywords
   '(
    ("\\(;+\\)\\(.*\\)$"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face))
    ;; (global label)?:? keyword
    ("^\\(\\(\\sw\\|\\s_\\|[\\.]*\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
     (1 font-lock-type-face) (3 font-lock-keyword-face nil t))
    ;; local label started from ".".
    ("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>:"
     1 font-lock-type-face)
	("#\\sw+"
     . font-lock-constant-face)
    ;; keywords
    ("\\s-+\\(lda\\|sta\\|ldx\\|stx\\|ldy\\|sty\\|and\\|ora\\|eor\\|bit\\|cmp\\|cpx\\|cpy\\|adc\\|sbc\\|asl\\|lsr\\|rol\\|ror\\|inc\\|dec\\|jmp\\|jsr\\|brk\\|rts\\|rti\\|php\\|plp\\|pha\\|pla\\|inx\\|dex\\|iny\\|dey\\|tax\\|txa\\|tay\\|tya\\|tsx\\|txs\\|sed\\|cld\\|sei\\|cli\\|sec\\|clc\\|clv\\|beq\\|bne\\|bmi\\|bpl\\|bcc\\|bcs\\|bvc\\|bvs\\|nop\\)\\s-+"
     1 font-lock-keyword-face)
    ;; implicit keywords
    (",[xy]" . font-lock-type-face)
    ;; built-in commands
    ("\\([.!]\\sw+\\|else\\|[{}]\\|macro\\|endm\\)"
     . font-lock-preprocessor-face)
    ;; macro definition
    ("!macro\\s-+\\(\\(\\sw\\|\\s_\\)+\\)"
     (1 font-lock-type-face))
    ;; macro application
    ("\\s-+\\(\\+\\sw+\\)\\s-+"
     (1 font-lock-keyword-face))
    ;; numeric constants
    ("\\b\\([$0-9][0-9a-fA-F]*\\)"
     . font-lock-constant-face))

  "Additional expressions to highlight in Assembler mode.")

;;;###autoload
(define-derived-mode kick-mode prog-mode "KickAssembler"
  "Major mode for editing typical assembler code.
Features a private abbrev table and the following bindings:

\\[kick-colon]\toutdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]\ttab to next tab stop.
\\[kick-newline]\tnewline, then tab to next tab stop.
\\[kick-comment]\tsmart placement of assembler comments.

The character used for making comments is set by the variable
`kick-comment-char' (which defaults to `?\\;').

Alternatively, you may set this variable in `kick-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on kick mode runs the hook `kick-mode-hook' at the end of initialization.

Special commands:
\\{kick-mode-map}"
  (setq local-abbrev-table kick-mode-abbrev-table)
  (set (make-local-variable 'font-lock-defaults) '(kick-font-lock-keywords))
  (set (make-local-variable 'tab-always-indent) nil)

  (use-local-map (nconc (make-sparse-keymap) kick-mode-map))
  (set-syntax-table (make-syntax-table kick-mode-syntax-table))
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'comment-start-skip)
       "\\(?:\\s<+\\|/[/*]+\\)[ \t]*")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(\\s>\\|\\*+/\\)")
  (set (make-local-variable 'comment-end) "")
  (setq fill-prefix "\t"))

(defun kick-indent-line ()
  "Auto-indent the current line."
  (interactive)
  (let* ((savep (point))
		 (indent (condition-case nil
					 (save-excursion
					   (forward-line 0)
					   (skip-chars-forward " \t")
					   (if (>= (point) savep) (setq savep nil))
					   (max (kick-calculate-indentation) 0))
				   (error 0))))
    (if savep
		(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun kick-calculate-indentation ()
  (or
   ;; Flush labels to the left margin.
   (and (looking-at "\\(\\sw\\|\\s_\\)+:") 0)
   ;; Same thing for `;;;' comments.
   (and (looking-at "\\s<\\s<\\s<") 0)
   ;; Simple `;' comments go to the comment-column.
   (and (looking-at "\\s<\\(\\S<\\|\\'\\)") comment-column)
   ;; The rest goes at the first tab stop.
   (or (indent-next-tab-stop 0))))

;; Obsolete since Emacs-22.1.
(defalias 'kick-newline 'newline-and-indent)

(provide 'kick-mode)

;;; kick-mode.el ends here
