;; Install and configure lsp-mode
(use-package lsp-mode
  :straight t
  :hook (
         ;; Enable LSP for specific modes
         (rust-mode . lsp-deferred)
         (nix-mode . lsp-deferred)
         (emacs-lisp-mode . lsp-deferred)
         
         ;; LSP completion setup
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  
  :commands (lsp lsp-deferred)
  
  :custom
  ;; Disable default LSP completion provider
  (lsp-completion-provider :none)
  
  ;; Performance tuning
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)
  
  ;; Disable features that might conflict with Corfu
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil))

;; LSP-specific completion setup function
(defun my/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))
  
  ;; Optional: Configure the first word as flex filtered
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  
  ;; Use Cape to bust the cache and improve completions
  (setq-local completion-at-point-functions 
              (list 
               (cape-capf-buster #'lsp-completion-at-point)
               #'cape-file
               #'cape-dabbrev)))

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
  (corfu-quit-at-boundary t)     ;; Quit at completion boundary
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

;; Cape for flexible completion
(use-package cape
  :straight t
  :init
  ;; Add `completion-at-point-functions` hooks
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Enhanced completion styles with Orderless
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (buffer (styles orderless))
                                        (help (styles orderless)))
        orderless-component-separator #'orderless-escapable-split-on-space
        orderless-matching-styles '(orderless-initialism 
                                    orderless-prefixes 
                                    orderless-regexp)))

;; Optional: Rich annotations for completions
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Company-like completion for text and help modes
(setq-default
 ;; Enable Corfu in text and help modes
 corfu-auto-prefix 2
 corfu-auto-delay 0.25
 completion-category-overrides '((text (styles orderless))
                                 (help (styles orderless))))

;; Add hook for text-mode completions
(add-hook 'text-mode-hook
          (lambda ()
            (setq-local corfu-auto t)
            (setq-local completion-at-point-functions 
                        (list 
                         #'cape-dabbrev
                         #'cape-file
                         #'cape-elisp-block))))

;; Inherit theme colors for Corfu
(custom-set-faces
 '(corfu-default ((t :inherit default)))
 '(corfu-current ((t :inherit highlight))))
