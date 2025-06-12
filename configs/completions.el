;;; completions.el --- Enhanced completion setup -*- lexical-binding: t -*-

;;; Commentary:
;;; Combines LSP, Cape and Corfu for a comprehensive completion experience

;;; Code:
(electric-pair-mode 1)

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
