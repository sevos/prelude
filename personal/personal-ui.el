;;; package --- user interface customizations
;;; Commentary:
;;; Code:

(prelude-require-package 'monokai-theme)
;; load django theme
(load-theme 'monokai t)

;; disable ring bell
(setq ring-bell-function 'ignore)

(provide 'personal-ui)
;;; personal-ui ends here
