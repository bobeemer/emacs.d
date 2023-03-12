;; -*- coding: utf-8; lexical-binding: t; -*-

(defun turnon-keyfreq-mode ()
  "Turn on keyfreq."
  (interactive)
  ;; Fire up keyfreq a few seconds later to start up emacs faster
  (my-run-with-idle-timer 4 (lambda ()
                               (keyfreq-mode 1)
                               (keyfreq-autosave-mode 1))))

(with-eval-after-load 'keyfreq

(setq keyfreq-excluded-commands
      '(find-file
        counsel-find-file
        eval-buffer
        save-buffer
        ivy-switch-buffer
        dired-jump
        kill-all-but-current-buffer
        ))

  (my-write-to-missing-file "()" keyfreq-file))

;; And use keyfreq-show to see how many times you used a command.
;; It's recommended to use `keyfreq-mode' (could be in "~/.custom.el").
;; It's reported keyfreq is not compatible with `latex-mode'
;; @see https://github.com/redguardtoo/emacs.d/issues/767
(turnon-keyfreq-mode)

(provide 'init-keyfreq)
;;; init-keyfreq.el ends here
