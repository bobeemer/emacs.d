;; -*- coding: utf-8; lexical-binding: t; -*-

(add-hook 'haskell-mode-hook 'subword-mode)
(add-hook 'haskell-cabal-hook 'subword-mode)
(add-hook 'haskell-cabal-hook 'dante-mode)

(with-eval-after-load 'dante-mode
  (flycheck-add-next-checker 'haskell-dante
                             '(warning . haskell-hlint)))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)


;; Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Source code helpers
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(reformatter-define hindent
  :program "hindent"
  :lighter " Hin")

(defalias 'hindent-mode 'hindent-on-save-mode)

(reformatter-define ormolu
  :program "ormolu"
  :lighter " Orm")

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))

(provide 'init-haskell)
