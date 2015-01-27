;;; package --- editor customizations
;;; Commentary:
;;; Code:

;; enable clipboard
(require 'pbcopy)
(turn-on-pbcopy)

;;(setq guru-warn-only nil)


(prelude-require-package 'ag)

(prelude-require-package 'nav)

(prelude-require-package 'yasnippet)
(yas-global-mode)

(prelude-require-package 'zoom-window)

(prelude-require-package 'ace-jump-buffer)

(prelude-require-package 'ace-jump-mode)

(require 'mouse)
(xterm-mouse-mode t)


(require 'inertial-scroll)
(setq inertias-global-minor-mode-map
      (inertias-define-keymap
       '(
         ;; Mouse wheel scrolling
         ("<wheel-up>"   . inertias-down-wheel)
         ("<wheel-down>" . inertias-up-wheel)
         ("<mouse-4>"    . inertias-down-wheel)
         ("<mouse-5>"    . inertias-up-wheel)
         ;; Scroll keys
         ("<next>"  . inertias-up)
         ("<prior>" . inertias-down)
         ("C-v"     . inertias-up)
         ("M-v"     . inertias-down)
         ) inertias-prefix-key))
(setq inertias-initial-velocity-wheel 10.0)
(setq inertias-rebound-flash nil)
(inertias-global-minor-mode 1)


(require '6502-mode "6502-mode")
(add-to-list 'auto-mode-alist '("\\.s$" . 6502-mode))
(add-hook '6502-mode-hook
          (lambda ()
            (ad-disable-advice 'yank 'around 'yank-and-indent)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (electric-indent-mode -1)))

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line-buffer-id)))

(unless window-system
  (setq linum-format 'linum-format-func))
(global-linum-mode)

;; enable emmet
(prelude-require-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;; enable multicursor
(prelude-require-package 'multiple-cursors)

;; configure zoom window
(setq zoom-window-mode-line-color "#ae81ff")


(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode)[])



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
