;;; evil.el --- Evil Setup -*- lexical-binding: t -*-

;;; Commentary:
;;; Make Emacs vim like.

;;; Code:

;; Install a git UI
(use-package magit
  :straight t)

;; A banner that shows possible keybinds.
(use-package which-key
  :straight t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; Use Doom-Modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 1))

(custom-set-faces
  '(doom-modeline-bar ((t (:background "#000000" :inherit mode-line)))))

(use-package evil
  :straight t
  :after org
  :init
  (setq evil-want-keybinding nil)
  (setq display-line-numbers-type 'relative)
  (evil-mode 1)
  :config
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  (add-hook 'org-mode-hook 'evil-org-mode))

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

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode))

(use-package writeroom-mode
  :straight t)

;; Install and configure nerd-icons
(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :straight t
  :commands (nerd-icons-dired-mode))

;; Add sidebar for file exploration
(use-package dired-sidebar
  :straight t
  :config
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'nerd-icons)
  (setq dired-sidebar-width 30)
  (setq dired-free-space nil))

(add-hook 'dired-sidebar-mode-hook (lambda () (setq mode-line-format nil)))

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
;; Disable default completion help messages.
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
  "ff" 'project-find-file   ; Find files in the project
  "fg" 'consult-ripgrep   ; Search in files using 'ripgrep'
  "fo" 'consult-outline)   ; Search in current file

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

;; Dired for file exploration
(my-leader-def 'normal
  "e" 'dired-sidebar-toggle-sidebar)

;; Buffers
(my-leader-def 'normal
  "bq" 'kill-buffer
  "bb" 'consult-buffer)

;; Roam
(my-leader-def 'normal
  "rr" 'org-roam-node-find
  "rc" 'org-roam-capture
  "ri" 'org-roam-node-insert)

;; Org
(my-leader-def 'normal
  "os" 'org-insert-structure-template
  "ot" 'org-toggle-checkbox
  "od" 'org-todo
  "oo" 'org-open-at-point-global)

;; Toggles
(my-leader-def 'normal
  "tl" 'display-line-numbers-mode
  "tz" 'writeroom-mode)

(general-def 'insert
  "A-s" 'lsp-signature-help)

;; Terminal mode escape
(general-def 'normal
  "Esc" 'evil-normal-state)

;; Toggle Term
(use-package vterm
  :straight t
  :config
  (setq vterm-shell "/usr/bin/env fish"))

(defun close-window-on-exit (process event)
"Close the window when the PROCESS (Vterm) exits."
  (when (memq (process-status process) '(exit signal))
    (let ((win (get-buffer-window (process-buffer process))))
      (when win (delete-window win)))))

(defun close-window-on-exit-eshell ()
  "Close the Eshell window when Eshell exits."
  (when (eq major-mode 'eshell-mode)
    (let ((win (get-buffer-window (current-buffer))))
      (when win (delete-window win)))))

(defun toggle-eshell ()
  "Toggle an Eshell window below the current one, and close it when exited."
  (interactive)
  (let ((eshell-buffer (get-buffer "*eshell*")))
    (if (and eshell-buffer (get-buffer-window eshell-buffer t))
        (delete-window (get-buffer-window eshell-buffer t)) ;; Close if visible
      (progn
        (split-window-below)
        (other-window 1)
        (if eshell-buffer
            (switch-to-buffer eshell-buffer)
          (eshell))
        (add-hook 'eshell-exit-hook #'close-window-on-exit-eshell)))))

(defun toggle-vterm ()
  "Toggle a vterm window below the current one, and close it when exited."
  (interactive)
  (let ((vterm-buffer (get-buffer "*vterm*")))
    (if (and vterm-buffer (get-buffer-window vterm-buffer t))
        (delete-window (get-buffer-window vterm-buffer t)) ;; Close if visible
      (progn
        (split-window-below)
        (other-window 1)
        (if vterm-buffer
            (switch-to-buffer vterm-buffer)
          (vterm))
        (let ((proc (get-buffer-process (current-buffer))))
          (when proc
            (set-process-sentinel proc #'close-window-on-exit)))))))

;; Leader key bindings
(my-leader-def 'normal
  "tt" 'toggle-eshell  ;; Toggle Eshell with `SPC tt`
  "tv" 'toggle-vterm)  ;; Toggle Vterm with `SPC tv`

;;; evil.el ends here.
