;;; package --- personal erlang configuration
;;; Commentary:
;;; Code:

(prelude-require-packages '(erlang flymake flymake-cursor))

(eval-after-load "erlang"
  '(progn
     (require 'erlang-start)
     (require 'erlang-eunit)
     (require 'erlang-flymake)
     (erlang-flymake-only-on-save)))

;; take care about soft tabs
(add-hook 'erlang-mode-hook
          (lambda ()
            (setq-local flycheck-checkers '())
            (setq-default indent-tabs-mode nil)
            (setq erlang-compile-function 'inferior-erlang-compile)
            (modify-syntax-entry ?_ "w")
            (setq erlang-eunit-autosave t)
            (setq-default tab-width 4)))

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

;; connect erlang mode to specific file types
(dolist (r '("rebar.config$"
             ".erlang$"
             "\\.config$"
             "\\.rel$"))
  (add-to-list 'auto-mode-alist (cons r 'erlang-mode)))

;; define where put beam files
(setq erlang-compile-outdir "../ebin")
;; configure node names.. for erlang mode and distel
(setq erl-nodename-cache 'emacs@127.0.0.1)
(setq distel-modeline-node "emacs@127.0.0.1")

;; configure path to wrangler
(setq wrangler-path (expand-file-name "vendor/wrangler/elisp" prelude-dir))
(require 'wrangler)

;; enable helm everywhere!
(require 'prelude-helm-everywhere)

(defun flatten-list (list)
  (apply #'append list))

;; search for top project directory
(defun erlang-find-rebar-top-recr (dirname)
  (let* ((project-dir (or (locate-dominating-file dirname "rebar.config")
                          (locate-dominating-file dirname "Makefile"))))
    (if project-dir
        (let* ((parent-dir
                (file-name-directory (directory-file-name project-dir)))
               (top-project-dir
                (if (and parent-dir (not (string= parent-dir "/")))
                    (erlang-find-rebar-top-recr parent-dir)
                  nil)))
          (if top-project-dir
              top-project-dir
            project-dir))
      project-dir)))

(defun erlang-find-rebar-top ()
  (interactive)
  (let* ((dirname (file-name-directory (buffer-file-name)))
         (project-dir (erlang-find-rebar-top-recr dirname)))
    (if project-dir
        project-dir
      (erlang-flymake-get-app-dir))))

(defun erlang-directory-dirs (dir name)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".." "rel" "_rel"))
        (let ((absolute-path (expand-file-name (concat dir "/" file))))
          (when (file-directory-p absolute-path)
            (if (string= file name)
                (setq dirs (append
                            (cons absolute-path
                                  (erlang-directory-dirs absolute-path name))
                            dirs))
              (setq dirs (append
                          (erlang-directory-dirs absolute-path name)
                          dirs)))))))
    dirs))

(defun erlang-all-dirs-with-name (name)
  (append (list (concat (erlang-find-rebar-top) name))
          (erlang-directory-dirs (erlang-find-rebar-top) name)))

(defun erlang-get-deps-code-path-dirs ()
  (erlang-all-dirs-with-name "ebin"))

(defun erlang-get-deps-include-dirs ()
  (erlang-all-dirs-with-name "include"))

(setq erlang-flymake-get-code-path-dirs-function 'erlang-get-deps-code-path-dirs)
(setq erlang-flymake-get-include-dirs-function 'erlang-get-deps-include-dirs)

(add-hook 'erlang-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'inferior-erlang-compile)
            (setq inferior-erlang-machine-options (dynamic-inferior-erlang-machine-options))
            (setq erlang-compile-extra-opts (dynamic-erlang-compile-extra-opts))))

(defun dynamic-erlang-compile-extra-opts ()
  "Generate compile optins with all 'include' dirs."
  (append
   (mapcar (lambda (dir) (cons 'i dir))
           (erlang-get-deps-include-dirs))
   '(bin_opt_info debug_info (d . \'TEST\'))))

(defun dynamic-inferior-erlang-machine-options ()
  "Generate inferiror optins with all 'ebin' and 'include' dirs."
  (append
   (list "-name" "emacs@127.0.0.1")
   (flatten-list
    (mapcar (lambda (dir) (list "-pa" dir))
            (erlang-get-deps-code-path-dirs)))
   (flatten-list
    (mapcar (lambda (dir) (list "-i" dir))
            (erlang-get-deps-include-dirs)))))


(provide 'personal-erlang)
;;; personal-erlang.el ends here
