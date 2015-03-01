;;; package --- editor customizations
;;; Commentary:
;;; Code:

;; enable clipboard
(require 'pbcopy)
(turn-on-pbcopy)

(setq-default tab-width 4)
;; enable guru mode
(setq guru-warn-only nil)
;; better grep
(prelude-require-package 'ag)
;; better navigation
(prelude-require-package 'nav)
;; handling textmate snippets
(prelude-require-package 'yasnippet)
(yas-global-mode)
;; enable zoom window
(prelude-require-package 'zoom-window)
;; handle ace jumps
(prelude-require-package 'ace-jump-buffer)
(prelude-require-package 'ace-jump-mode)
;; enable progress bar via as nyan cat ;)
(prelude-require-package 'nyan-mode)
(setq nyan-bar-length 20)
(nyan-mode)

;; configure js tab width
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq tab-width 2
                   js2-basic-offset tab-width)))

;; enable subwords
(global-subword-mode 1)

;; enable mouse
(require 'mouse)
(xterm-mouse-mode t)

;; enable inertial scrolling
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
;; configurate speed and inertial
(setq inertias-initial-velocity-wheel 10.0)
(setq inertias-rebound-flash nil)
(inertias-global-minor-mode 1)

;; improve performance
(setq flx-ido-threshold 1000)

(add-to-list 'auto-mode-alist '("Procfile" . conf-mode))

(require 'smartparens-ruby)

;; handle better linum mode
(unless window-system
  (add-hook
   'linum-before-numbering-hook
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

(setq helm-recentf-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-locate-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t)
(setq helm-split-window-in-side-p       nil)

(prelude-require-package 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


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
