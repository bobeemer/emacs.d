;; -*- coding: utf-8; lexical-binding: t -*-

;; my screen is tiny, so I use minimum eshell prompt
(with-eval-after-load 'eshell
  (setq eshell-prompt-function
        (lambda ()
          (concat (getenv "USER") " $ "))))

(provide 'init-term-mode)
