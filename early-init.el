;;; early-init.el --- Set very early init. -*- lexical-binding: t -*-
;; -*- lexical-binding: t; -*-

;;; Commentary:
;; This file sets the very first things that happens during Emacs's launch.

;;; Code:
;; (setq default-frame-alist '((background-color . "#000000")))
(setq package-enable-at-startup nil)

;; Set default (fixed-pitch) font
(set-face-attribute 'default nil
                    :family "FantasqueSansM Nerd Font Mono"
                    :height 120) ;; Adjust size as needed

;; Set variable-pitch font
(set-face-attribute 'variable-pitch nil
                    :family "FantasqueSansM Nerd Font"
                    :height 120) ;; Adjust size as needed

;; (load-theme 'modus-vivendi t)
;; (set-face-attribute 'fringe nil :background nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default mode-line-format nil)

;; Frame Information
(setq default-frame-alist
      (append '((width                . 140)
                (left                 . 170)
                (top                  . 30 )
                (bottom               . 30 )
                (line-spacing         . 0  )
                (left-fringe          . 12 )
                (right-fringe         . 12 )
                (cursor-type          . box)
                (alpha                . 100))
              default-frame-alist))

;; Properly load and configure modus themes
(require 'modus-themes nil t)

;; Use customize-set-variable which properly handles not-yet-defined vars
(customize-set-variable 'modus-themes-bold-constructs t)
(customize-set-variable 'modus-themes-italic-constructs t)
(customize-set-variable 'modus-themes-org-blocks 'gray-background)

;; Custom palette overrides to ensure fringes match background exactly
(customize-set-variable 'modus-themes-common-palette-overrides
						'((fringe unspecified)
						  (bg-fringe unspecified)))

;; Define separate overrides for each theme
(customize-set-variable 'modus-vivendi-palette-overrides
                        '((border "#191919")))

(customize-set-variable 'modus-operandi-palette-overrides
                        '((border "#f0f0f0")))

;; Additional customizations for thinner borders
(set-frame-parameter nil 'internal-border-width 1)

;; Load theme
(load-theme 'modus-vivendi t)

;;; early-init.el ends here
