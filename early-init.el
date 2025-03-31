;;; early-init.el --- Set very early init.
;; -*- lexical-binding: t; -*-

;;; Commentary:
;; This file sets the very first things that happens during Emacs's launch.

;;; Code:
(setq default-frame-alist '((background-color . "#000000")))
(setq package-enable-at-startup nil)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; early-init.el ends here
