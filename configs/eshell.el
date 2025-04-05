;;; dashboard.el --- Eshell Configuration with Fixed Clear Command and Vertico Integration -*- lexical-binding: t -*-

;;; Commentary:
;;; Eshell what else?

;;; Code:

;; Remove the default Eshell banner
(setq eshell-banner-message "")

;; Create a custom lambda prompt function
(defun my-eshell-prompt ()
  (let ((pwd (eshell/pwd)))
    (concat
     (propertize (if (string= pwd (getenv "HOME"))
                     "~"
                   (file-name-nondirectory pwd))
                 'face '(:foreground "#5fafff"))
     (propertize " λ " 'face '(:foreground "#ff875f"))
     )))

;; Set the custom prompt
(setq eshell-prompt-function #'my-eshell-prompt)
(setq eshell-prompt-regexp "^.* λ ")

;; Fixed clear command that doesn't cause duplicate prompts
(defun eshell/clear ()
"Clear the eshell buffer properly without causing duplicate prompts."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Move point to end but don't send input
    (goto-char (point-max))))

;; Basic Eshell config
(use-package eshell
  :hook ((eshell-mode . (lambda ()
                          ;; Protect prompt in evil mode
                          (when (bound-and-true-p evil-local-mode)
                            (setq-local evil-move-cursor-back nil)
                            (setq-local evil-move-beyond-eol t))
                          
                          ;; Make sure the prompt is read-only
                          (add-hook 'eshell-after-prompt-hook
                                    (lambda ()
                                      (let ((inhibit-read-only t))
                                        (add-text-properties
                                         (line-beginning-position)
                                         (point)
                                         '(read-only t field output inhibit-line-move-field-capture t))))
                                    nil t)
                          
                          ;; Improved completion setup for vertico
                          (setq-local completion-category-defaults nil)
                          
                          ;; Disable Corfu in Eshell
                          ;; (when (bound-and-true-p corfu-mode)
                          ;;   (corfu-mode -1)
							))))
  :config
  ;; Better history management
  (setq eshell-history-size 10000)
  (setq eshell-save-history-on-exit t)
  (setq eshell-hist-ignoredups t)
  
  ;; Use the default completion system for tab with vertico integration
  (setq eshell-default-completion-function #'completion-at-point)
  (define-key eshell-mode-map [tab] 'completion-at-point)
  (define-key eshell-mode-map (kbd "TAB") 'completion-at-point)
  
  ;; Improve pcomplete settings for better vertico integration
  (setq pcomplete-ignore-case t)
  (setq pcomplete-autolist t)
  (setq pcomplete-termination-string "")
  (setq pcomplete-cycle-completions nil)
  (setq pcomplete-complete-with-space nil)
  
  ;; Simple settings for better completion behavior
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-cmpl-dir-ignore nil)
  
  ;; Visual commands that should run in term mode
  (setq eshell-visual-commands
        '("btm" "less" "more" "vim" "nvim" "top" "nano" "watch"))
  (setq eshell-visual-subcommands
        '(("git" "log" "diff" "show")))
  (setq eshell-term-name "xterm-256color")

;; Syntax highlighting
(use-package eshell-syntax-highlighting
  :straight t
  :after esh-mode
  :config (eshell-syntax-highlighting-global-mode +1))

;; Aliases setup
(setq eshell-aliases-file "~/.emacs.d/eshell/aliases")
(unless (file-exists-p eshell-aliases-file)
  (with-temp-file eshell-aliases-file
    (insert "alias ll ls -lah --color=auto\n")
    (insert "alias grep rg\n")
    (insert "alias find fd\n")
    (insert "alias cat bat\n")
    (insert "alias .. cd ..\n")
    (insert "alias ... cd ../..\n")
    (insert "alias g git\n")
    (insert "alias btm btm --color=always\n")
    (insert "alias htop btm\n")
    (insert "alias clear clear-scrollback\n")
    (insert "alias l ls -la\n")))

;; Cape integration for better completions
(use-package cape
  :after eshell
  :config
  ;; Add cape completion sources
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Enable completion styles for better matching
  (setq completion-styles '(basic partial-completion flex)))

;; Evil mode specific adjustments for better prompt compatibility
(with-eval-after-load 'evil
  (when (boundp 'evil-collection-eshell-next-prompt-on-insert)
    (setq evil-collection-eshell-next-prompt-on-insert t))
  
  (defun eshell-evil-insert-next ()
    "Go to the next prompt and enter insert state."
    (interactive)
    (goto-char eshell-last-output-end)
    (evil-insert-state))
  
  (evil-define-key 'normal eshell-mode-map
    (kbd "G") 'eshell-evil-insert-next
    (kbd "I") 'eshell-evil-insert-next)
  
  ;; Prevent backspace from modifying prompt
  (evil-define-key 'insert eshell-mode-map
    (kbd "<backspace>") (lambda ()
                          (interactive)
                          (if (> (point) eshell-last-output-end)
                              (call-interactively 'backward-delete-char)
                            (message "Cannot delete prompt")))))
