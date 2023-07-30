;; -*- coding: utf-8; lexical-binding: t; -*-

;; https://github.com/redguardtoo/emacs.d/issues/208
;; (blink-cursor-mode -1)


(defun my-prorated-addition-theme ()
  "Load custom theme in different hours."
  (require 'spaceway-theme)
  (setq currrnt-hour
        (string-to-number (format-time-string "%H" (current-time))))
  (if (member currrnt-hour (number-sequence 6 18))
      (setq current-theme '(spaceway))
    (setq current-theme '(spaceway)))
  (load-theme (car current-theme) t))

(when (window-system)
  (my-run-with-idle-timer 2 #'my-prorated-addition-theme))

;; (use-package spaceway-theme
;;   :ensure nil
;;   :load-path "test/spaceway/"
;;   :config
;;   (global-hl-line-mode t)
;;   (set-cursor-color "#dc322f")
;;   (when 1
;;     (set-frame-parameter (selected-frame) 'alpha '(100 100))
;;     (set-frame-parameter (selected-frame) 'alpha-background 100)
;;     (add-to-list 'default-frame-alist '(alpha-background 90))
;;     (add-to-list 'default-frame-alist '(alpha 100 100)))
;;   (load-theme 'spaceway t)
;;   (setenv "SCHEME" "dark"))


(provide 'init-theme)
;;; init-theme.el ends here
