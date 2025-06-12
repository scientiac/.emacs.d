;;; completions.el --- Enhanced completion setup -*- lexical-binding: t -*-

;;; Commentary:
;;; Combines LSP, Cape and Corfu for a comprehensive completion experience

;;; Code:
(electric-pair-mode 1)

;; Cape for flexible completion - load before LSP mode to ensure priorities
(use-package cape
  :straight t
  :init
  ;; Add cape sources to completion-at-point-functions
  ;; Use append to ensure they're considered after other backends
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block t)
  
  :config
  ;; Fix file completion for relative paths
  (setq dabbrev-check-all-buffers nil))

;; Corfu for in-buffer completions
(use-package corfu
  :straight t
  :init
  (global-corfu-mode 1)
  :custom
  ;; Optional customizations
  (corfu-cycle t)                ;; Enable cycling for completions
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Don't quit at completion boundary to show multiple sources
  (corfu-quit-no-match t)        ;; Quit if no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . corfu-insert)
        ("C-g" . corfu-quit)
        ("<escape>" . corfu-insert-separator)))

;; Optional: Rich annotations for completions
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Company-like completion for text and help modes
(setq-default
 ;; Enable Corfu in text and help modes
 corfu-auto-prefix 2
 corfu-auto-delay 0.25)

;; Inherit theme colors for Corfu
(custom-set-faces
 '(corfu-default ((t :inherit default)))
 '(corfu-current ((t :inherit highlight))))

;; RustMODE
(use-package rustic
  :straight t
  :config
  (setq rustic-format-on-save t)
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rust-mode-treesitter-derive t)
  (rustic-cargo-use-last-stored-arguments t))

;;; completions.el ends here
