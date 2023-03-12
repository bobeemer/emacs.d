;; -*- coding: utf-8; lexical-binding: t; -*-

;; org-clock is studying.

;; {{ use key ">" or "<" control tree lever.
(defun org-demote-or-promote (&optional is-promote)
  "Demote or promote current org tree."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (unless (or (region-active-p)
                (let ((line (thing-at-point 'line t)))
                  (and (string-match-p "^\\*+ $" line) ;; is node only one spaced
                       (= (point) (- (point-max) (length line))) ;; is line at EOF
                       )))
      (org-mark-subtree)))
  (if is-promote (org-do-promote) (org-do-demote)))

;;}}

(setq org-cycle-include-plain-lists 'integrate) ; 将列表视为heading

;; open link in org mode
;; from : https://stackoverflow.com/questions/4506249/how-can-i-make-emacs-org-mode-open-links-to-sites-in-google-chrome
;; (setq browse-url-browser-function 'browse-url-generic
;;      browse-url-generic-program "chromium-browser")
;; if window system is windows or macosx
;; (setq browse-url-browser-function 'browse-url-default-windows-browser)
;; (setq browse-url-browser-function 'browse-url-default-macosx-browser)
(defun my-org-link-broswer-setup ()
  "Set default broswer when click org link. "
  (cond
   (*linux*
    (setq browse-url-browser-function 'browse-url-generic
     browse-url-generic-program "google-chrome-stable"))
   (*mac*
    (setq browse-url-browser-function 'browse-url-ddefault-macosx-browser))
   (or *win64* *cygwin*
       (setq browse-url-browser-function 'browse-url-default-windows-browser))))

(my-org-link-broswer-setup)


(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (org-modern-mode 1)))

(provide 'init-org)
;;; init-org.el ends here
