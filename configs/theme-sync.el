;;; theme-sync.el --- Sync Emacs theme with system theme -*- lexical-binding: t; -*-
;;; Commentary:
;; This file handles synchronization between system theme and Emacs theme via DBus.
;;; Code:

(require 'dbus)

(defun iac/set-theme-from-dbus-value (value)
  "Set the appropriate theme according to the color-scheme setting VALUE.
   0 = light theme, 1 = dark theme"
  (message "DBus color-scheme value is %s" value)
  (if (equal value '1)
      (progn 
        (message "Switching to dark theme (modus-vivendi)")
        (load-theme 'modus-vivendi t))
    (progn 
      (message "Switching to light theme (modus-operandi)")
      (load-theme 'modus-operandi t))))

(defun iac/freedesktop-color-scheme-changed (path var value)
  "DBus handler to detect when the freedesktop color-scheme has changed.
PATH and VAR identify the setting, VALUE is the new value."
  (when (and (string-equal path "org.freedesktop.appearance")
             (string-equal var "color-scheme"))
    (iac/set-theme-from-dbus-value (car value))))

(defun iac/setup-theme-sync ()
  "Set up synchronization between system theme and Emacs theme for future changes."
  ;; Only register for future changes - initial theme already set in early-init.el
  (dbus-register-signal
   :session "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'iac/freedesktop-color-scheme-changed))

(provide 'theme-sync)
;;; theme-sync.el ends here
