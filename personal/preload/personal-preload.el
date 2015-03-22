;;; package --- personal preload
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq prelude-theme 'monokai)

(if (eq window-system 'ns)
    (progn (toggle-scroll-bar -1)
           (tool-bar-mode -1)
           (setq ns-pop-up-frames nil)
           (set-face-attribute 'default nil :font "Anonymous Pro-16")))

(server-force-delete)
(server-start)

(if (fboundp 'fringe-mode)
    (fringe-mode 8))

(provide 'personal-preload)
;;; personal-preload.el ends here
