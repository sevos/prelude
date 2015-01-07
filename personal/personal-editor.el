;;; package --- editor customizations
;;; Commentary:
;;; Code:

;; enable clipboard
(require 'pbcopy)
(turn-on-pbcopy)

;; enable silver searcher
(prelude-require-package 'ag)

;; enable yasnippet
(prelude-require-package 'yasnippet)

;; enable zoom-window
(prelude-require-package 'zoom-window)

;; enable ace-jump-buffer - C-J
(prelude-require-package 'ace-jump-buffer)

;; enable ace-jump-mode - C-j
(prelude-require-package 'ace-jump-mode)

;; enable helm everywhere!
(require 'prelude-helm-everywhere)

;; disable lockfiles
(setq create-lockfiles  nil)

(provide 'personal-editor)
;;; personal-editor.el ends here
