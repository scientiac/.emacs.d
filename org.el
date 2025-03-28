;; Set normal font (non-mono version)
(setq nano-font-family "FantasqueSansM Nerd Font")

;; Org-mode font configuration
(with-eval-after-load 'org
  (set-face-attribute 'org-default nil 
                      :font nano-font-family)
  
  ;; Ensure italics work
  (set-face-attribute 'italic nil 
                      :slant 'italic 
                      :font (concat nano-font-family " Italic")))

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
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (setq-local org-hide-emphasis-markers nil)
            (org-toggle-pretty-entities)))

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (setq-local org-hide-emphasis-markers t)
            (org-toggle-pretty-entities)))

(global-org-modern-mode)
