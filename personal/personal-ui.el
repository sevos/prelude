;;; package --- user interface customizations
;;; Commentary:
;;; Code:

;; disable ring bell
(setq ring-bell-function 'ignore)

(custom-theme-set-faces
 'smart-mode-line-dark
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil)))
 '(mode-line-inactive ((t :foreground "gray60" :background "#333333" :inverse-video nil)))
 '(mode-line     ((t :foreground "gray60" :background nil :inverse-video nil)))
 '(sml/global    ((t :foreground "gray50" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "White")))
 '(sml/filename  ((t :inherit sml/global :foreground "#eab700" :weight bold)))
 '(sml/prefix    ((t :inherit sml/global :foreground "#bf6000")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
 '(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
 '(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))

(provide 'personal-ui)
;;; personal-ui ends here
