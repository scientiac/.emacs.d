;; Reset Fringes
(setq default-frame-alist
      (append (list'(left-fringe  . 18))))

;; Hide the toolbar, menubar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Fonts Setup
(set-face-attribute 'bold nil :weight 'bold)

;; Scroll Smoothly
(setq redisplay-dont-pause t
      auto-window-vscroll nil
      scroll-margin 1
      scroll-step 4
      scroll-conservatively 10000
      fast-but-imprecise-scrolling nil
      jit-lock-defer-time 0
      scroll-preserve-screen-position 1)

(set-face-attribute 'mode-line-inactive nil :underline "#434C5E" :foreground "#2E3440")
(set-face-attribute 'mode-line nil :underline "#434C5E" :foreground "#2E3440")
(add-hook 'echo-area-clear-hook 'nano-theme--minibuffer)

;; Fonts for Org
(defun iac/org-headings (&optional frame)
  "Set Org mode heading font sizes, optionally for a specific FRAME."
  (with-selected-frame (or frame (selected-frame))
    (custom-set-faces
     '(org-level-1 ((t (:height 1.5))))
     '(org-level-2 ((t (:height 1.4))))
     '(org-level-3 ((t (:height 1.3))))
     '(org-level-4 ((t (:height 1.2))))
     '(org-level-5 ((t (:height 1.1)))))))

;; Run on startup and whenever a new frame is created
(add-hook 'after-init-hook #'iac/org-headings)
(add-hook 'after-make-frame-functions #'iac/org-headings)
