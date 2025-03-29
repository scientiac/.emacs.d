(use-package which-key
  :straight t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))

(use-package evil
  :straight t
  :after org
  :init
  (setq evil-want-keybinding nil)
  (setq display-line-numbers-type 'relative)
  (evil-mode 1)
  :config
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after (evil)
  :config
  (evil-commentary-mode 1)
  :bind (
    :map evil-normal-state-map
    ("gc" . evil-commentary)))

(use-package writeroom-mode
  :straight t)

(use-package vterm
  :straight t
  :config
  (setq vterm-shell "/usr/bin/env zsh"))

;; Install and configure nerd-icons
(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Add Neotree for file exploration, and ensure it loads after nerd-icons
(use-package neotree
  :straight t
  :after nerd-icons
  :config
  (setq neo-smart-open t)
  (setq neo-theme 'nerd-icons)
  (setq neo-window-width 25))

;; For Keybinds
(use-package general
  :straight t)

;; Define leader key
(general-create-definer my-leader-def
  :prefix "SPC")

;; Insert mode movement (equivalent to Neovim)
(general-def 'insert
  "C-h" 'left-char
  "C-j" 'next-line
  "C-k" 'previous-line
  "C-l" 'right-char
  "C-c" 'evil-normal-state)

;; Normal mode keymaps
(general-def 'normal
  "C-c" 'evil-ex-nohighlight
  "C-s" 'save-buffer
  "C-h" 'evil-window-left
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right
  "C-d" (lambda () (interactive) (evil-scroll-down nil) (recenter))
  "C-u" (lambda () (interactive) (evil-scroll-up nil) (recenter)))

;; Resizing splits
(general-def 'normal
  "A-k" (lambda () (interactive) (enlarge-window 2))
  "A-j" (lambda () (interactive) (shrink-window 2))
  "A-h" (lambda () (interactive) (enlarge-window-horizontally 2))
  "A-l" (lambda () (interactive) (shrink-window-horizontally 2)))

;; Visual mode mappings
(general-def 'visual
  "??" (lambda () (interactive) (evil-yank (region-beginning) (region-end)) (describe-symbol (current-kill 0)))
  "?/" (lambda () (interactive) (evil-yank (region-beginning) (region-end)) (swiper (current-kill 0))))

;; Install consult with straight.el
;; Disable default completion help messages 
(setq completion-show-help nil)

;; Install and configure vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t)
  (setq vertico-count 10)
  :bind (:map vertico-map
              ("<tab>" . vertico-next)
              ("<backtab>" . vertico-previous)
              ("RET" . vertico-exit)))

;; Install and configure consult
(use-package consult
  :straight t
  :config
  ;; Enable automatic previews for consult-fd
  (consult-customize
   consult-fd
   :preview-key 'any)  ; Automatically preview files as you navigate
  )

;; Your existing keybindings
(my-leader-def 'normal
  "ff" 'consult-fd         ; Find files using 'fd' utility
  "fg" 'consult-ripgrep)   ; Search in files using 'ripgrep'

;; Optional: Orderless for better matching
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Optional: Marginalia for annotations
(use-package marginalia
  :straight t
  :init
  (marginalia-mode 1))

;; Neotree instead of dired for file exploration
(my-leader-def 'normal
  "e" 'neotree-toggle  ; Toggle Neotree sidebar
  "bq" 'kill-buffer
  "bb" 'consult-buffer)

;; Zen mode (focus mode equivalent)
(my-leader-def 'normal
  "tz" 'writeroom-mode)

;; Terminal bindings
;; Function to toggle vterm window
(defun my-toggle-vterm ()
  "Toggle a vterm window below the current one."
  (interactive)
  (let ((vterm-buffer (get-buffer "*vterm*")))
    (if (and vterm-buffer (get-buffer-window vterm-buffer t))
        ;; If vterm is visible, delete its window
        (delete-window (get-buffer-window vterm-buffer t))
      ;; If vterm isn’t visible, create it
      (progn
        (split-window-below)
        (other-window 1)
        (if vterm-buffer
            (switch-to-buffer vterm-buffer)
          (vterm))))))

;; Leader key binding for toggling vterm
(my-leader-def 'normal
  "tt" 'my-toggle-vterm)

(general-def 'insert
  "A-s" 'lsp-signature-help)

;; Terminal mode escape
(general-def 'normal
  "Esc" 'evil-normal-state)
