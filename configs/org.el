;;; org.el --- Configuration for Org and Roam -*- lexical-binding: t; -*-
;;; Commentary:
;; This file configures Org mode and Org Roam using straight.el
;; for fully declarative package management.
;;; Code:

;; Org mode configuration
(use-package org
  :straight t
  :defer t
  :custom
  ;; Basic Org configuration
  (org-directory "~/Organism")
  (org-default-notes-file (concat org-directory "/agenda.org"))
  ;; Edit settings
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  ;; Org styling, hide markup etc.
  (org-link-descriptive nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :config
  ;; Org-mode font configuration
  (custom-set-faces
   ;; Emphasis styles
   '(org-italic ((t (:slant italic))))
   '(org-bold ((t (:weight bold))))
   '(org-underline ((t (:underline t))))
   '(org-strikethrough ((t (:strike-through t))))
   ;; Headings
   '(org-level-1 ((t (:height 1.5))))
   '(org-level-2 ((t (:height 1.4))))
   '(org-level-3 ((t (:height 1.3))))
   '(org-level-4 ((t (:height 1.2))))
   '(org-level-5 ((t (:height 1.1))))))

;; Org Roam configuration
(use-package org-roam
  :straight t
  :after org
  :custom
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-directory (file-truename "~/Organism/roam"))
  (org-roam-db-location "~/.cache/emacs/org-roam/org-roam.db")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      ""
      :target (file+head "${slug}.org"
               "#+title: ${title}\n#+created: <%<%Y-%m-%d %H:%M:%S>>\n")
      :unnarrowed t)))
  :config
  ;; Keeps the DB in sync
  (org-roam-db-autosync-mode))

;; Org Modern configuration
(use-package org-modern
  :straight t
  :after org
  :custom
  (org-modern-fold-stars '(("▶" . "▼")
                           ("▷" . "▽")
                           ("▸" . "▾")
                           ("▹" . "▿")
                           ("⯈" . "⯆")))
  (org-modern-hide-stars 'leading)
  (org-modern-checkbox '((32 . "󰄱")
                         (88 . "󰱒")
                         (45 . "󰡖")))
  :hook
  ;; Enable org-modern-mode only in normal mode (when using evil)
  (evil-normal-state-entry . (lambda ()
                              (when (derived-mode-p 'org-mode)
                                (org-modern-mode 1))))
  (evil-insert-state-entry . (lambda ()
                               (when (derived-mode-p 'org-mode)
                                 (org-modern-mode -1)))))

;;; org.el ends here
