  (with-eval-after-load 'org
    (custom-set-faces
     '(org-level-1 ((t (:height 1.5))))
     '(org-level-2 ((t (:height 1.4))))
     '(org-level-3 ((t (:height 1.3))))
     '(org-level-4 ((t (:height 1.2))))
     '(org-level-5 ((t (:height 1.1))))))

;; Ensure packages are installed
(setq evil-want-keybinding nil)
(use-package evil-org
  :straight t)

;; Org mode minimal setup
(require 'org)
(require 'org-modern)

(setq org-modern-fold-stars '(
                              ("⯈" . "⯆")
                              ("▶" . "▼")
                              ("▷" . "▽")
                              ("▹" . "▿")
                              ("▸" . "▾")))

(setq org-modern-hide-stars 'leading)

(setq org-modern-checkbox
      '((32 . "󰄱")
        (88 . "󰱒")
        (45 . "󰡖")))

;; Basic Org configuration
(setq org-directory "~/Org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

(global-org-modern-mode)
