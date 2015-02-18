;;; package --- personal clojure configuration
;;; Commentary:
;;; Code:

(prelude-require-packages '(clojure-mode cider))
(dolist (r '("\\.cljs$" "\\.cljx$" "\\.edn$"))
  (add-to-list 'auto-mode-alist (cons r 'clojure-mode)))

;; cider
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-popup-stacktraces t)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-history-file "~/.cider-history")
(setq cider-repl-wrap-history t)

(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(provide 'personal-clojure)
;;; personal-clojure.el ends here
