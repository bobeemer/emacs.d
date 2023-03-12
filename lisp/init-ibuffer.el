;; -*- coding: utf-8; lexical-binding: t; -*-

(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000)
      (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000)
      (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t
      (format "%8d" (buffer-size)))))

  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-display-summary nil)

  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("code" (or (mode . emacs-lisp-mode)
                             (mode . c-mode)
                             (mode . lisp-mode)
                             (mode . c++-mode)
                             (mode . cmake-mode)))

                 ("dired" (or (mode . dired-mode)
                              (mode . sr-mode)))

                 ("planner" (or (name . "^\\*Calendar\\*$")
                                (name . "^diary$")
                                (mode . muse-mode)
                                (mode . org-mode)
                                (mode . org-agenda-mode)))

                 ("emacs" (or (name . "^\\*scratch\\*$")
                              (name . "^\\*Messages\\*$")))
		 ))))

  (defun ibuffer-mode-hook-setup ()
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook 'ibuffer-mode-hook-setup)

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

  (setq ibuffer-filter-group-name-face 'font-lock-doc-face))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
