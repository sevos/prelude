;;; package --- personal kick configuration
;;; Commentary:
;;; Code:

(defun build-c64-file ()
  "Compiles project."
  (interactive)
  (let ((file buffer-file-name))
    (shell-command (concat "cd .. && make start "
                           (file-name-sans-extension
                            (file-name-nondirectory file))))))

(require 'kick-mode)
(dolist (r '("\\.s$" "\\.asm$"))
  (add-to-list 'auto-mode-alist (cons r 'kick-mode)))

(font-lock-add-keywords
 'kick-mode
 '(("^[^\n]\\{40\\}\\(.*\\)$"
    1 font-lock-warning-face prepend)))

(eval-after-load 'kick-mode
  '(progn
     (define-key kick-mode-map (kbd "C-c C-c") 'build-c64-file)))

(provide 'personal-kick)
;;; personal-kick.el ends here
