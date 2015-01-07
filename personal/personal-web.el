;;; package --- personal web mode configuration
;;; Commentary:
;;; Code:

;; enable web mode
(prelude-require-package 'web-mode)

;; connect it to specific file types
(dolist (r '("\\.erb\\'"
             "\\.scss\\'"
             "\\.hbs\\'"
             "\\.html?\\'"))
  (add-to-list 'auto-mode-alist (cons r 'web-mode)))

;; configure identation
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(provide 'personal-web)
;;; personal-web.el ends here
