;;; package --- user interface customizations
;;; Commentary:
;;; Code:

;; disable ring bell
(setq ring-bell-function 'ignore)

(toggle-scroll-bar -1)
(tool-bar-mode -1)

(blink-cursor-mode 1)
;; (setq-default cursor-type 'bar)
(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (modify-frame-parameters frame
                                      '((vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)))))
(custom-theme-set-faces
 'smart-mode-line-dark
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil)))
 '(mode-line-inactive ((t :foreground "gray60" :background nil :inverse-video nil)))
 '(mode-line     ((t :foreground "gray60" :background nil :inverse-video nil)))
 '(sml/global    ((t :foreground "gray50" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "White")))
 '(sml/filename  ((t :inherit sml/global :foreground "#eab700" :weight bold)))
 '(sml/prefix    ((t :inherit sml/global :foreground "#c1f161")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
 '(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
 '(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))

(setq ns-use-native-fullscreen nil)
(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'personal-ui)
;;; personal-ui ends here
