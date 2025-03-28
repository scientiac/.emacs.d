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
        `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory))))
  
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
  (setq save-place-file (expand-file-name "places" user-emacs-directory)
        save-place-forget-unreadable-files t
        save-place-save-skipped nil))

;; Temporary and backup file management
(setq
 ;; Centralize backup, autosave, and lock files
 backup-directory-alist 
 `((".*" . ,(expand-file-name "backup/" user-emacs-directory)))
 
 auto-save-file-name-transforms
 `((".*" ,(expand-file-name "autosave/" user-emacs-directory) t))
 
 ;; Prevent file clutter
 create-lockfiles nil  ; Disable .# lock files
 make-backup-files t   ; Enable backups
 backup-by-copying t  ; Don't modify original file permissions
 
 ;; Keep many backups, clean old ones
 kept-new-versions 10
 kept-old-versions 5
 delete-old-versions t
 version-control t)

;; Create backup and autosave directories if they don't exist
(let ((backup-dir (expand-file-name "backup/" user-emacs-directory))
      (autosave-dir (expand-file-name "autosave/" user-emacs-directory)))
  (mkdir backup-dir t)
  (mkdir autosave-dir t))
