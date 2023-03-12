;; -*- coding: utf-8; lexical-binding: t; -*-

;; https://github.com/redguardtoo/emacs.d/issues/208
;; (blink-cursor-mode -1)

(defun my-prorated-addition-theme ()
    "Load custom theme in different hours."
    (setq currrnt-hour
          (string-to-number (format-time-string "%H" (current-time))))
    (if (member currrnt-hour (number-sequence 6 18))
              (setq current-theme '(modus-vivendi-tinted))
      (setq current-theme '(modus-vivendi-tinted)))
    (load-theme (car current-theme) t))

(when (window-system)
  (my-run-with-idle-timer 2 #'my-prorated-addition-theme))

(provide 'init-theme)
;;; init-theme.el ends here
