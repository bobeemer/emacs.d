;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'flycheck
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;(add-hook 'prog-mode-hook 'global-flycheck-mode)


(provide 'init-flycheck)
