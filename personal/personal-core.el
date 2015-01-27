;;; package --- personal core functions
;;; Commentary:
;;; Code:

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (let ((yas-fallback-behavior nil))
            (unless (yas-expand)
              (dabbrev-expand nil)))
        (indent-for-tab-command)))))

;; stops hidding windows on escape...
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(defun split-window-transpose (&optional arg)
  "Switch between horizontal and vertical 2-window layouts"
  (interactive "p")
  (if (eq 2 (count-windows))
      (let* ((sel-w (selected-window))
             (fw (frame-width (window-frame sel-w)))
             (ww (window-width))
             (left-top-right-bottom (window-edges sel-w))
             (sel-left (nth 0 left-top-right-bottom))
             (sel-top (nth 1 left-top-right-bottom))
             (is-left-top)
             (vis-w)
             (other-w))
        (progn
          ;; save other buffer
          (defun get-other-window ()
            (walk-windows
             (function (lambda(w)
                         (if (not (equal w (selected-window)))
                             (push w vis-w)))))
            (car vis-w))
          (setq other-b (window-buffer (get-other-window)))

          ;; clear other window
          (delete-other-windows)

          ;; determine which way to transpose
          (if (= fw ww)
              (split-window-horizontally)
            (split-window-vertically))

          ;; determine whether it's left/top or right/bottom
          (if (or (> sel-left 0) (> sel-top 0))
              ;; right bottom






              (progn
                (set-window-buffer (selected-window) other-b)
                (other-window 1))
            (progn
              (setq other-w (get-other-window))
              (set-window-buffer other-w other-b)))))))

(defun personal-split-window-vertically-and-other-window ()
  "Split window vertically and go to it."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun personal-split-window-horizontally-and-other-window ()
  "Split window horizontally and go to it."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun personal-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun personal-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (personal-indent-buffer)
        (message "Indented buffer.")))))

(provide 'personal-core)
;;; personal-core.el ends here
