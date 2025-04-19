;;; completions.el --- Enhanced completion setup -*- lexical-binding: t -*-

;;; Commentary:
;;; Combines LSP, Cape and Corfu for a comprehensive completion experience

;;; Code:
(electric-pair-mode 1)

(use-package flycheck
  :straight t
  :commands global-flycheck-mode
  :init
  (setq flycheck-mode-globals '(not rust-mode rustic-mode))
  (global-flycheck-mode))

;; Inline flychek mode.
(use-package flycheck-inline
  :straight t)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

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
  (setq cape-file-directory-must-exist nil)
  (setq cape-file-attributes nil))

;; Install and configure lsp-mode
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  
  :custom
  ;; Use capf completion provider to work better with cape
  (lsp-completion-provider :capf)
  
  ;; Performance tuning
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)
  
  ;; Disable features that might conflict with Corfu
  (lsp-enable-snippet nil)
  (lsp-warn-no-matched-clients nil)
  (lsp-enable-symbol-highlighting nil)
  
  :config
  ;; Make sure LSP completions don't overshadow Cape completions
  (advice-add 'lsp-completion-at-point :around
              (lambda (orig-fun &rest args)
                (let ((result (apply orig-fun args)))
                  (if result
                      ;; Move cape completions into the priority list
                      (let ((capfs (remove #'lsp-completion-at-point completion-at-point-functions)))
                        (setcdr (last result) (list :company-prefix-length (car result) 
                                                   :exit-function #'ignore
                                                   :annotation-function #'ignore
                                                   :exclusive 'no))
                        result)
                    ;; Fall back to other completion mechanisms
                    (run-hook-with-args-until-success
                     'completion-at-point-functions))))))

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
        ("<escape>" . corfu-insert-separator))
  :config
  ;; Setup orderless for LSP completions
  (defun corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (add-hook 'lsp-completion-mode-hook #'corfu-setup-lsp))

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

;; Treesitter
(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))

;; RustMODE
(use-package rustic
  :straight t
  :config
  (setq rustic-format-on-save t)
  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rust-mode-treesitter-derive t)
  (rustic-cargo-use-last-stored-arguments t))

;; Markdown Mode
(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :custom
  (markdown-fontify-code-blocks-natively t))

;; nix-mode with tree-sitter + lsp support
(use-package nix-ts-mode
  :straight t
  :mode "\\.nix\\'"
  :hook (nix-ts-mode . lsp-deferred))

(use-package nix-mode
:straight t
:after lsp-mode
:ensure t
:hook
(nix-mode . lsp-deferred)
:custom
(lsp-disabled-clients '((nix-mode . nix-nil)))
:config
(setq lsp-nix-nixd-server-path "nixd"
      lsp-nix-nixd-formatting-command [ "nixfmt" ]
      lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
      lsp-nix-nixd-nixos-options-expr "(builtins.getFlake \"/home/nb/nixos\").nixosConfigurations.mnd.options"
      lsp-nix-nixd-home-manager-options-expr "(builtins.getFlake \"/home/nb/nixos\").homeConfigurations.\"nb@mnd\".options"))

;;; completions.el ends here
