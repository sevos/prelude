;;; package --- custom keybindings
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-q")   'bury-buffer)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x 2") 'personal-split-window-vertically-and-other-window)
(global-set-key (kbd "C-x 3") 'personal-split-window-horizontally-and-other-window)
(global-set-key (kbd "C-x 4") 'balance-windows)
(global-set-key (kbd "C-x 5") 'personal-swap-windows)
(global-set-key (kbd "C-x 6") 'split-window-transpose)
(global-set-key (kbd "C-x 7") 'zoom-window-zoom)
(global-set-key (kbd "M-i")   'personal-indent-region-or-buffer)
(global-set-key (kbd "M-=")   'er/expand-region)
(global-set-key (kbd "M--")   'er/contract-region)
(global-set-key (kbd "C-c m l") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(key-chord-define-global "jj" 'nav-toggle-left)
(global-set-key (kbd "TAB") 'smart-tab)
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

(provide 'personal-keybindings.el)
;;; personal-keybindings.el ends here
