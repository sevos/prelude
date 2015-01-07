;;; package - personal ruby config
;;; Commentary:
;;; Code:

(prelude-require-package 'robe)

;; connect robe-mode to ruby-mode
(add-hook 'ruby-mode-hook 'robe-mode)

;; handle underscore as part of word
(add-hook 'ruby-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-z") 'inf-ruby-console-auto)
            ))

(provide 'personal-ruby)
;;; personal-ruby.el ends here
