;;; nano.el --- Configuration for Nano Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file manages UI of my Emacs.

;;; Code:
(set-face-attribute 'bold nil :weight 'bold)

;; No startup  screen
(setq inhibit-startup-screen t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(setq initial-frame-alist default-frame-alist)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; Scroll Smoothly
(setq auto-window-vscroll nil
	  mouse-wheel-progressive-speed nil
      scroll-margin 5
      scroll-step 4
      scroll-conservatively 10000
      fast-but-imprecise-scrolling t
      jit-lock-defer-time 0
      scroll-preserve-screen-position 1)

;;; ui.el ends here.
