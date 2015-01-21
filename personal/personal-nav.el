;;; package --- user interface customizations
;;; Commentary:
;;; Code:

(require 'nav)
(defun nav-toggle-left ()
  "Toggle the nav panel on left."
  (interactive)
  (if (nav-current-buffer-is-nav)
      (nav-unsplit-window-horizontally)
    (if (nav-left-neighbor-is-nav)
	(progn
	  (windmove-left)
	  (nav-unsplit-window-horizontally))
      (progn
        (when (nav-current-window-has-left-window-neighbor)
          (windmove-left))
        (when (nav-current-window-has-left-window-neighbor)
          (windmove-left))
        (nav)))))

(provide 'personal-nav)
;;; personal-nav ends here
