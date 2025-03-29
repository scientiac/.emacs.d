;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Straight Init
(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Config Reload    
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "<f5>") 'reload-init-file)

;; install NANO using straight.el
(load (expand-file-name "configs/nano.el" user-emacs-directory) t)
;; Dashboard
(load (expand-file-name "configs/dashboard.el" user-emacs-directory) t)
;; Install org mode
(load (expand-file-name "configs/org.el" user-emacs-directory) t)
;; Load Evil Mode
(load (expand-file-name "configs/evil.el" user-emacs-directory) t)
;; Load Completions Mode
(load (expand-file-name "configs/completions.el" user-emacs-directory) t)
;; Load Persistence Mode
(load (expand-file-name "configs/persistence.el" user-emacs-directory) t)
;; Load UI configuration
(load (expand-file-name "configs/ui.el" user-emacs-directory) t)
