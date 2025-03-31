;;; nano.el --- Configuration for Nano Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Nano Emacs, including font settings, themes,
;; and layout management.

;;; Code:

;; Install NANO
(straight-use-package
  '(nano :type git :host github :repo "rougier/nano-emacs"))

;; Define font variables
(defvar nano-font-size 11 "Font size for Nano Emacs.")
(defvar nano-font-family-monospaced "FantasqueSansM Nerd Font Mono" "Monospaced font family for Nano Emacs.")

(require 'nano-layout)

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

;; Theme
(require 'nano-faces)
(require 'nano-theme)

(require 'nano-base-colors)
(defun nano-theme-set-dark ()
  "Apply dark Nano theme base."
  ;; Colors from Nord theme at https://www.nordtheme.com
  (setq frame-background-mode     'dark)
  (setq nano-color-foreground "#EBDBB2") ;; Gruvbox light fg
  (setq nano-color-background "#000000") ;; Gruvbox dark hard bg
  (setq nano-color-highlight  "#3C3836") ;; Gruvbox dark bg1
  (setq nano-color-critical   "#FB4934") ;; Gruvbox red
  (setq nano-color-salient    "#83A598") ;; Gruvbox blue
  (setq nano-color-strong     "#EBDBB2") ;; Gruvbox light fg
  (setq nano-color-popout     "#FE8019") ;; Gruvbox orange
  (setq nano-color-subtle     "#151515") ;; Gruvbox dark bg2
  (setq nano-color-faded      "#928374") ;; Gruvbox gray
  ;; to allow for toggling of the themes.
  (setq nano-theme-var "dark"))

(defun nano-theme-set-light ()
  "Apply light Nano theme base."
  ;; Colors from Gruvbox Light theme
  (setq frame-background-mode    'light)
  (setq nano-color-foreground "#3C3836") ;; Gruvbox light fg0
  (setq nano-color-background "#F9F5D7") ;; Gruvbox light hard bg
  (setq nano-color-highlight  "#EBDBB2") ;; Gruvbox light bg1
  (setq nano-color-critical   "#9D0006") ;; Gruvbox light red
  (setq nano-color-salient    "#076678") ;; Gruvbox light blue
  (setq nano-color-strong     "#282828") ;; Gruvbox light black
  (setq nano-color-popout     "#AF3A03") ;; Gruvbox light orange
  (setq nano-color-subtle     "#D5C4A1") ;; Gruvbox light bg2
  (setq nano-color-faded      "#7C6F64") ;; Gruvbox light gray
  ;; to allow for toggling of the themes.
  (setq nano-theme-var "light"))

(cond
 ((member "-default" command-line-args) t)
 ((member "-light" command-line-args) (nano-theme-set-light))
 (t (nano-theme-set-dark)))
(call-interactively 'nano-refresh-theme)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Splash (optional)
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash))

(set-face-attribute 'mode-line-inactive nil :underline "#151515" :foreground "#1D2021")
(set-face-attribute 'mode-line nil :underline "#151515" :foreground "#1D2021")
(add-hook 'echo-area-clear-hook 'nano-theme--minibuffer)

;;; nano.el ends here.
