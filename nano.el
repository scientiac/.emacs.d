;; Install NANO
(straight-use-package
  '(nano :type git :host github :repo "rougier/nano-emacs"))

;; Use NANO
(setq nano-font-size 11)
(setq nano-font-family-monospaced "FantasqueSansM Nerd Font Mono")
;; Path to nano emacs modules (mandatory)
(add-to-list 'load-path "/Users/rougier/Documents/GitHub/nano-emacs")

;; Default layout (optional)
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

;; Terminal colors for Gruvbox Dark Hard theme
(setq ansi-color-names-vector
      ["#1D2021"    ; black (using actual Gruvbox bg instead of #000000)
       "#FB4934"    ; red
       "#B8BB26"    ; green (added from Gruvbox palette)
       "#FABD2F"    ; yellow (added from Gruvbox palette)
       "#83A598"    ; blue
       "#D3869B"    ; magenta (added from Gruvbox palette)
       "#8EC07C"    ; cyan (added from Gruvbox palette)
       "#EBDBB2"])  ; white

;; Terminal colors for Gruvbox Dark Hard theme (dark variants only)
(setq ansi-color-names-vector
      ["#1D2021"    ; black (dark bg)
       "#CC241D"    ; dark red 
       "#98971A"    ; dark green
       "#D79921"    ; dark yellow
       "#458588"    ; dark blue
       "#B16286"    ; dark magenta
       "#689D6A"    ; dark cyan
       "#A89984"])  ; dark white/gray

;; Set terminal colors when in a terminal
(defun setup-terminal-colors ()
  (unless (display-graphic-p)
    (setq xterm-color-names
          ["#1D2021" "#CC241D" "#98971A" "#D79921"
           "#458588" "#B16286" "#689D6A" "#A89984"])
    
    ;; Still using darker colors even for "bright" variants
    (setq xterm-color-names-bright
          ["#3C3836" "#9D0006" "#79740E" "#B57614"
           "#076678" "#8F3F71" "#427B58" "#7C6F64"])))

;; Run setup when Emacs is in terminal mode
(add-hook 'tty-setup-hook 'setup-terminal-colors)

(cond
 ((member "-default" command-line-args) t)
 ((member "-light" command-line-args) (nano-theme-set-light))
 (t (nano-theme-set-dark)))
(call-interactively 'nano-refresh-theme)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))
  
;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash))

;; Help (optional)
(unless (member "-no-help" command-line-args)
  (require 'nano-help))

(set-face-attribute 'mode-line-inactive nil :underline "#151515" :foreground "#1D2021")
(set-face-attribute 'mode-line nil :underline "#151515" :foreground "#1D2021")
(add-hook 'echo-area-clear-hook 'nano-theme--minibuffer)
