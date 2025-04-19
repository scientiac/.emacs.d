;;; early-init.el --- Set very early init. -*- lexical-binding: t -*-

;;; Commentary:
;; This file sets the very first things that happens during Emacs's launch.

;;; Code:
(setq default-frame-alist '((undecorated . t)))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Properly load and configure modus themes
(require 'modus-themes nil t)

;; Use customize-set-variable which properly handles not-yet-defined vars
(customize-set-variable 'modus-themes-bold-constructs t)
(customize-set-variable 'modus-themes-italic-constructs t)
(customize-set-variable 'modus-themes-org-blocks 'gray-background)

;; Custom palette overrides to ensure fringes match background exactly
(customize-set-variable 'modus-themes-common-palette-overrides
                        '((fringe unspecified)
                          (bg-line-number-inactive unspecified)
                          (bg-line-number-active unspecified)
                          (bg-fringe unspecified)))

;; Define separate overrides for each heme
(customize-set-variable 'modus-vivendi-palette-overrides
                        '((border "#191919")
						  (bg-mode-line-active unspecified)
						  (bg-mode-line-inactive unspecified)
						  (border-mode-line-inactive unspecified)
						  (border-mode-line-active unspecified)))

(customize-set-variable 'modus-operandi-palette-overrides
                        '((border "#f0f0f0")
						  (bg-mode-line-active unspecified)
						  (bg-mode-line-inactive unspecified)
						  (border-mode-line-inactive unspecified)
						  (border-mode-line-active unspecified)))

;; Additional customizations for thinner borders
(set-frame-parameter nil 'internal-border-width 1)

;; Check system theme once during startup to avoid flashing
(require 'dbus)
(let ((theme-value
       (condition-case nil
           (let ((result (dbus-call-method
                          :session "org.freedesktop.portal.Desktop"
                          "/org/freedesktop/portal/desktop"
                          "org.freedesktop.portal.Settings"
                          "Read"
                          "org.freedesktop.appearance"
                          "color-scheme")))
             (caar result))
         (error 0))))
  (if (equal theme-value 1)
      (load-theme 'modus-vivendi t)
    (load-theme 'modus-operandi t)))

;; (setq default-frame-alist '((background-color . "#000000")))
(setq package-enable-at-startup nil)
;; Set default (fixed-pitch) font
(set-language-environment "UTF-8")
(set-face-attribute 'default nil
                    :family "FantasqueSansM Nerd Font Mono"
                    :height 110) ;; Adjust size as needed
;; Set variable-pitch font
(set-face-attribute 'variable-pitch nil
                    :family "FantasqueSansM Nerd Font"
                    :height 110) ;; Adjust size as needed
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (setq-default mode-line-format nil)
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

;;; early-init.el ends here
