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

(eval-after-load '6502-mode
  '(progn
     (define-key 6502-mode-map (kbd "C-c C-c") 'build-c64-file)))

;; enable 6502
(require '6502-mode "6502-mode")
(add-to-list 'auto-mode-alist '("\\.s$" . 6502-mode))
(add-hook '6502-mode-hook
          (lambda ()
            (ad-disable-advice 'yank 'around 'yank-and-indent)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (electric-indent-mode -1)))

(provide 'personal-asm)
;;; personal-asm.el ends here
