;;; package --- editor customizations
;;; Commentary:
;;; Code:

;; enable clipboard
(require 'pbcopy)
(turn-on-pbcopy)

(setq guru-warn-only nil)

;; enable silver searcher
(prelude-require-package 'ag)

;; enable nav
 (prelude-require-package 'nav)

;; enable yasnippet
(prelude-require-package 'yasnippet)

;; enable zoom-window
(prelude-require-package 'zoom-window)

;; enable ace-jump-buffer - C-c-J
(prelude-require-package 'ace-jump-buffer)

;; enable ace-jump-mode - C-c-j
(prelude-require-package 'ace-jump-mode)

(global-linum-mode)

;; enable emmet
(prelude-require-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; enable multicursor
(prelude-require-package 'multiple-cursors)

;; configure zoom window
(setq zoom-window-mode-line-color "#ae81ff")

;; enable helm everywhere!
(helm-autoresize-mode t)
(require 'prelude-helm-everywhere)

(setq helm-split-window-in-side-p       nil
      helm-move-to-line-cycle-in-source t)

;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions using C-z
(define-key helm-map (kbd "C-z")  'helm-select-action)



;; disable lockfiles
(setq create-lockfiles  nil)

(provide 'personal-editor)
;;; personal-editor.el ends here
