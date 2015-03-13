;;; Code:

(defgroup kick nil
  "Mode for editing assembler code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defvar kick-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    st)
  "Syntax table used while in Kick mode.")

(defvar kick-mode-abbrev-table nil
  "Abbrev table used while in kick mode.")
(define-abbrev-table 'kick-mode-abbrev-table ())

(defvar kick-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Note that the comment character isn't set up until kick-mode is called.
    map)
  "Keymap for Kick mode.")

(defconst kick-font-lock-keywords
  (append
   '(("^\\(\\(\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
      (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t))
     ;; label started from ".".
     ("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>:"
      1 font-lock-function-name-face)
     ("^\\((\\sw+)\\)?\\s +\\(\\(\\.?\\sw\\|\\s_\\)+\\(\\.\\sw+\\)*\\)"
      2 font-lock-keyword-face)
     ;; directive started from ".".
     ("^\\(\\.\\(\\sw\\|\\s_\\)+\\)\\>[^:]?"
      1 font-lock-keyword-face)
     ;; %register
     ("%\\sw+" . font-lock-variable-name-face))
   cpp-font-lock-keywords)
 "Additional expressions to highlight in KickAssembler mode.")

;;;###autoload
(define-derived-mode kick-mode prog-mode "KickAssembler"
  (setq local-abbrev-table kick-mode-abbrev-table)
  (set (make-local-variable 'font-lock-defaults) '(kick-font-lock-keywords))
  ;; Stay closer to the old TAB behavior (was tab-to-tab-stop).
  (set (make-local-variable 'tab-always-indent) nil)

  (run-hooks 'kick-mode-set-comment-hook)
  (use-local-map (nconc (make-sparse-keymap) kick-mode-map))
  (set-syntax-table (make-syntax-table kick-mode-syntax-table))
  (set (make-local-variable 'comment-start) "(/\/\*|\/\/)")
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'comment-start-skip)
       "\\(?:\\s<+\\|/[/*]+\\)[ \t]*")
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(\\s>\\|\\*+/\\)")
  (set (make-local-variable 'comment-end) "(|\*\/)")
  (setq fill-prefix "\t"))

(provide 'kick-mode)

;;; kick-mode.el ends here
