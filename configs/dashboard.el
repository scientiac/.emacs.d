;;; dashboard.el --- A blank page on startup. -*- lexical-binding: t -*-
;;; Commentary:
;;; A clean slate on startup that stays centered when resizing.
;;; Code:

(defun nano-dashboard ()
  (interactive)
  
  ;; Prevent dashboard from showing if files are open
  (if (eq 0 (length (cl-loop for buf in (buffer-list)
                              if (buffer-file-name buf)
                              collect (buffer-file-name buf))))
      (let ((dashboard-buffer (get-buffer-create "*dashboard*")))
        (with-current-buffer dashboard-buffer
          (erase-buffer)
          
          ;; Buffer local settings
          (setq cursor-type nil)
          ;; (global-hide-mode-line-mode 1)

          ;; Use special-mode as parent to get some convenient behaviors
          (special-mode)
          
          ;; Create local hook for window configuration changes
          (add-hook 'window-configuration-change-hook
                    'nano-dashboard-center-text
                    nil t)
          
          ;; Add window size adjustment hook
          (add-hook 'window-size-change-functions
                    (lambda (_) (nano-dashboard-center-text))
                    nil t)
          
          ;; Initial centering
          (nano-dashboard-center-text)
          
          ;; Display the buffer
          (display-buffer-same-window dashboard-buffer nil)
          
          ;; Any key kills the buffer
          (local-set-key [t] 'kill-this-buffer)
          
          ;; Suppress startup messages
          (run-with-idle-timer 0.05 nil (lambda() (message nil)))))))

(defun nano-dashboard-center-text ()
  "Center dashboard text based on current window dimensions."
  (with-current-buffer (get-buffer "*dashboard*")
    (let* ((height (window-body-height nil))
           (width (window-body-width nil))
           (padding-center (max 0 (round (/ (- height 4) 2)))))
      
      ;; Make buffer editable temporarily
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Set fill column for proper centering
        (setq fill-column width)
        
        ;; Vertical padding to center
        (insert-char ?\n padding-center)
        
        ;; Main title with zen symbol
        (insert (propertize "GNU Emacs / Λ Z E N" 'face 'nano-face-strong))
        (center-line)
        (insert "\n")
        
        ;; Subtitle
        (insert (propertize "Emacs made Z E N" 'face 'nano-face-faded))
        (center-line)
        
        ;; Return to the top and make read-only again
        (goto-char (point-min))))))

;; Fallback for nano-face if not defined
(unless (facep 'nano-face-strong)
  (defface nano-face-strong
    '((t :weight bold))
    "Strong face for nano theme"
    :group 'nano-faces))

(unless (facep 'nano-face-faded)
  (defface nano-face-faded
    '((t :foreground "#9e9e9e"))
    "Faded face for nano theme"
    :group 'nano-faces))

;; Replace nano-splash with our custom dashboard
(add-hook 'window-setup-hook 'nano-dashboard)
;; Completely hide the echo area messages
(setq inhibit-message t)
(setq message-log-max nil)

;;; dashboard.el ends here
