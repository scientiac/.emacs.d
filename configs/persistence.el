;;; persistence.el --- Manage persistent state in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file manages persistent state in Emacs, including undo history,
;; cursor position, and temporary file management.

;;; Code:

(setq global-auto-revert-mode 1)

(use-package undo-tree
  :straight t
  :after evil
  :init
  (global-undo-tree-mode)
  :config
  ;; Ensure undo-tree works with Evil mode
  (setq evil-undo-system 'undo-tree)
  
  ;; Persistent undo history configuration
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "persist/undo-tree-history" user-emacs-directory))))
  
  ;; Ensure undo history is loaded
  (setq undo-tree-enable-undo-in-region t)
  
  ;; Make sure history is compressed and saved
  (setq undo-tree-history-compression-level 9)
  
  ;; Explicitly set Evil undo keys
  :bind (:map evil-normal-state-map
              ("u" . undo-tree-undo)
              ("C-r" . undo-tree-redo))
  
  ;; Ensure undo-tree is always on
  :hook (after-init . global-undo-tree-mode))

;; Persistent cursor and file position
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1)
  :config
  (setq save-place-file (expand-file-name "persist/places" user-emacs-directory)
        save-place-forget-unreadable-files t
        save-place-save-skipped nil))

;; Temporary and backup file management
(setq
 ;; Centralize backup, autosave, and lock files
 backup-directory-alist
 `((".*" . ,(expand-file-name "persist/backup/" user-emacs-directory)))
 
 auto-save-file-name-transforms
 `((".*" ,(expand-file-name "persist/autosave/" user-emacs-directory) t))
 
 ;; Prevent file clutter
 create-lockfiles nil  ; Disable .# lock files
 make-backup-files t   ; Enable backups
 backup-by-copying t  ; Don't modify original file permissions
 
 ;; Keep many backups, clean old ones
 kept-new-versions 10
 kept-old-versions 5
 delete-old-versions t
 version-control t)

;; Have Emacs maintain a history of mini buffer commands and opened files.
(savehist-mode 1)
(recentf-mode 1)

;; Create backup and autosave directories if they don't exist
(let ((persist-dir (expand-file-name "persist/" user-emacs-directory))
      (backup-dir (expand-file-name "persist/backup/" user-emacs-directory))
      (autosave-dir (expand-file-name "persist/autosave/" user-emacs-directory))
      (undotree-dir (expand-file-name "persist/undo-tree-history/" user-emacs-directory)))
  (mkdir persist-dir t)
  (mkdir backup-dir t)
  (mkdir undotree-dir t)
  (mkdir autosave-dir t))

(provide 'persistence)
;;; persistence.el ends here
