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


;(eval-after-load 'kick-mode
;  '(progn
;     (define-key kick-mode-map (kbd "C-c C-c") 'build-c64-file)))

(provide 'personal-asm)
;;; personal-asm.el ends here
