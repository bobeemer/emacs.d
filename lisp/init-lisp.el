;; -*- coding: utf-8; lexical-binding: t; -*-

(defun show-scratch-buffer-message ()
  (let* ((fortune-prog (or (executable-find "fortune-zh")
                           (executable-find "fortune"))))
    (cond
     (fortune-prog
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; " ; comment each line
        (replace-regexp-in-string
         "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9]m *\\)" ""    ; remove trailing linebreak
         (shell-command-to-string fortune-prog)))))
     (t
      (concat ";; Happy hacking "
              (or user-login-name "")
              " - Emacs loves you!\n\n")))))

(setq-default initial-scratch-message (show-scratch-buffer-message))

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(defun lisp-mode-hook-setup ()
  "Enable features useful in any Lisp mode."
  (enable-paredit-mode)
  (rainbow-delimiters-mode t)
  (my-ensure 'eldoc)
  (turn-on-eldoc-mode))

(let* ((hooks '(lisp-mode-hook
                inferior-lisp-mode-hook
                lisp-interaction-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook 'lisp-mode-hook-setup)))

(defun elisp-mode-hook-setup ()
    (my-ensure 'eldoc)
    (turn-on-eldoc-mode)
    (enable-paredit-mode)
    (rainbow-delimiters-mode t)

    ;; Locally set `hippie-expand' completion functions for use with Emacs Lisp.
    (make-local-variable 'hippie-expand-try-functions-list)
    (push 'try-complete-lisp-symbol hippie-expand-try-functions-list)
    (push 'try-complete-lisp-symbol-partially hippie-expand-try-functions-list)
    (checkdoc-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook-setup)

(provide 'init-lisp)
;;; init-lisp.el ends here
