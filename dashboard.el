(defun nano-dashboard ()
  "Create a minimal dashboard inspired by nano-splash"
  (interactive)
  
  ;; Prevent dashboard from showing if files are open
  (if (eq 0 (length (cl-loop for buf in (buffer-list)
                              if (buffer-file-name buf)
                              collect (buffer-file-name buf))))
      (let* ((dashboard-buffer (get-buffer-create "*dashboard*"))
             (height (round (- (window-body-height nil) 1)))
             (width (round (window-body-width nil)))
             (padding-center (+ (/ height 2) 1)))
        
        (with-current-buffer dashboard-buffer
          (erase-buffer)
          
          ;; Buffer local settings
          (setq mode-line-format nil)
          (setq header-line-format nil)
          (setq cursor-type nil)
          (setq line-spacing 0)
          (setq vertical-scroll-bar nil)
          (setq horizontal-scroll-bar nil)
          (setq fill-column width)
          
          ;; Vertical padding to center
          (insert-char ?\n padding-center)
          (insert "\n")
          (insert (propertize "GNU Emacs / N Λ N O" 'face 'nano-face-strong))
          (center-line)
          (insert "\n")
          (insert (propertize "Emacs made simple" 'face 'nano-face-faded))
          (center-line)
          
          (goto-char 0)
          (read-only-mode t)
          (local-set-key [t] 'kill-this-buffer)
          (display-buffer-same-window dashboard-buffer nil)
          
          ;; Suppress startup messages
          (run-with-idle-timer 0.05 nil (lambda() (message nil)))))))

;; Replace nano-splash with our custom dashboard
(add-hook 'window-setup-hook 'nano-dashboard)

;; Suppress startup messages
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Completely suppress startup message, especially for Evil mode
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Completely hide the echo area messages
(setq inhibit-message t)
(setq message-log-max nil)

;; Advice to suppress message function
(defadvice message (around my-suppress-message activate)
  (let ((inhibit-message t))
    ad-do-it))
