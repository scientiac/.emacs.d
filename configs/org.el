;;; org.el --- Configuration for Org and Roam -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Org mode.
;; and layout management.

;;; Code:
(require 'org)

;; Basic Org configuration
(setq org-directory "~/Organism")
(setq org-default-notes-file (concat org-directory "/agenda.org"))

;; Setup Org Roam
(require 'org-roam)
(setq org-roam-db-gc-threshold most-positive-fixnum)

;; Setup roam dirs.
(setq org-roam-directory (file-truename "~/Organism/roam")
    org-roam-db-location "~/.cache/emacs/org-roam/org-roam.db"
    org-roam-completion-everywhere t)

(setq org-roam-capture-templates
  '(("d" "default" plain
     ""
     :target (file+head "${slug}.org"
              "#+title: ${title}\n#+created: <%<%Y-%m-%d %H:%M:%S>>\n")
     :unnarrowed t)))

;; Keeps the DB in sync.
(org-roam-db-autosync-mode)

;; Org mode minimal setup
(require 'org-modern)

(setq org-modern-fold-stars '(
                              ("▶" . "▼")
                              ("▷" . "▽")
                              ("▸" . "▾")
                              ("▹" . "▿")
                              ("⯈" . "⯆")))

(setq org-modern-hide-stars 'leading)

(setq org-modern-checkbox
      '((32 . "󰄱")
        (88 . "󰱒")
        (45 . "󰡖")))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-link-descriptive 0
 org-hide-emphasis-markers t
 org-pretty-entities t)

;; Prettify Fonts only on normal mode.
(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (setq-local org-hide-emphasis-markers nil)
            (org-toggle-pretty-entities)
            (org-toggle-link-display)))
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (setq-local org-hide-emphasis-markers t)
            (org-toggle-pretty-entities)
            (org-toggle-link-display)))


;; Enable org-modern-mode only on normal mode.
(add-hook 'evil-normal-state-entry-hook
          (lambda ()
            (when (derived-mode-p 'org-mode)
              (org-modern-mode 1))))
(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (when (derived-mode-p 'org-mode)
              (org-modern-mode -1))))

;; Org-mode font configuration
(with-eval-after-load 'org
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

;;; org.el ends here.
