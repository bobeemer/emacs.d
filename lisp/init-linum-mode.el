;; -*- coding: utf-8; lexical-binding: t; -*-

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(defvar my-linum-inhibit-modes
  '(
    eshell-mode
    dired-mode)
  "Major modes without line number.")

;; I don't care Emacs 25 performance any more
;; (setq display-line-numbers-width 1) toggle width
(defun display-line-numbers-mode-hook-setup ()
  (setq display-line-numbers (not (memq major-mode my-linum-inhibit-modes))))

(add-hook 'display-line-numbers-mode-hook 'display-line-numbers-mode-hook-setup)
(my-run-with-idle-timer 2 #'global-display-line-numbers-mode)

(provide 'init-linum-mode)
;;; init-linum-mode.el ends here
