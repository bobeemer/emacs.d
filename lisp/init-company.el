;; -*- coding: utf-8; lexical-binding: t; -*-

(add-hook 'after-init-hook 'global-company-mode)

(defvar my-company-select-by-number-p t
  "User can press number key to select company candidate.")

(defvar my-company-zero-key-for-filter nil
  "If t, pressing 0 calls `company-filter-candidates' per company's status.
If `my-company-select-by-number-p' is nil, this flag is ignored. ")

(with-eval-after-load 'company

  ;; company changed the default key bindings, un-comment below code to restore original key bindings
  ;; @see https://github.com/company-mode/company-mode/wiki/Tips-%26-tricks/_compare/5ea840d^...5ea840d

  ;; (define-key company-active-map (kbd "C-n") nil)
  ;; (define-key company-active-map (kbd "C-p") nil)
  ;; (define-key company-active-map (kbd "M-n") #'company-select-next)
  ;; (define-key company-active-map (kbd "M-p") #'company-select-previous)

  (defun my-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k))
           (n (if (equal k "0") 10 (string-to-number k))))
      (cond
       ((or (cl-find-if (lambda (s) (string-match re s)) company-candidates)
            (> n (length company-candidates))
            (looking-back "[0-9]" (line-beginning-position)))
        (self-insert-command 1))

       ((and (eq n 10) my-company-zero-key-for-filter)
        (company-filter-candidates))

       (t
        (company-complete-number n)))))

  ;; @see https://github.com/company-mode/company-mode/issues/348
  ;; a way to sort completions by usage frequency
  (company-statistics-mode)

  ;; (company-quickhelp-mode)

  ;; company-mode completion backend for CMake scripts.
  ;; (push 'company-cmake company-backends)
  ;; (push 'company-c-headers company-backends)

  ;; @see https://oremacs.com/2017/12/27/company-numbers/
  ;; Using digits to select company-mode candidates
  (when my-company-select-by-number-p
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'my-company-number))
     (number-sequence 0 9))))

  (company-ctags-auto-setup)

  (setq company-show-numbers t
    ;; characters "/ ) . , ;"to trigger auto commit
    ;; company-auto-commit t
    ;; company-auto-commit-chars '(92  41 46 44 59)
    company-dabbrev-downcase nil
    ;; make previous/next selection in the popup cycles
    company-selection-wrap-around t
    ;; Some languages use camel case naming convention,
    ;; so company should be case sensitive.
    company-dabbrev-ignore-case nil
    ;; press M-number to choose candidate
    company-idle-delay 0.2
    company-clang-insert-arguments nil
    company-require-match nil
    ;; @see https://github.com/company-mode/company-mode/issues/146
    company-tooltip-align-annotations t
    ;; Press SPACE will accept the highlighted candidate and insert a space
  ;; "M-x describe-variable company-auto-complete-chars" for details.
  ;; So that's BAD idea.
    company-auto-complete nil)

   (setq company-global-modes
      '(not
        minibuffer-inactive-mode)))

(with-eval-after-load 'company-ispell
  (defun my-company-ispell-available-hack (orig-func &rest args)
    ;; in case evil is disabled
    (my-ensure 'evil-nerd-commenter)
    (cond
     ((and (derived-mode-p 'prog-mode)
           (or (not (company-in-string-or-comment)) ; respect advice in `company-in-string-or-comment'
               ;; I renamed the api in new version of evil-nerd-commenter
               (not (if (fboundp 'evilnc-pure-comment-p) (evilnc-pure-comment-p (point))
                      (evilnc-is-pure-comment (point)))))) ; auto-complete in comment only
      ;; only use company-ispell in comment when coding
      nil)
     (t
      (apply orig-func args))))
  (advice-add 'company-ispell-available :around #'my-company-ispell-available-hack))

;; {{ setup company-ispell
(defun toggle-company-ispell ()
  "Toggle company-ispell."
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (push 'company-ispell company-backends)
    (message "company-ispell enabled!"))))

(defun company-ispell-setup ()
  ;; @see https://github.com/company-mode/company-mode/issues/50
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (push 'company-ispell company-backends)
    ;; @see https://github.com/redguardtoo/emacs.d/issues/473
    (cond
     ((and (boundp 'ispell-alternate-dictionary)
           ispell-alternate-dictionary)
      (setq company-ispell-dictionary ispell-alternate-dictionary))
     (t
       (setq company-ispell-dictionary (file-truename (concat my-emacs-dir "misc/english-words.txt")))))))

;; message-mode use company-bbdb.
;; So we should NOT turn on company-ispell
(add-hook 'org-mode-hook 'company-ispell-setup)
;; }}

(provide 'init-company)
;;; init-company.el ends here
