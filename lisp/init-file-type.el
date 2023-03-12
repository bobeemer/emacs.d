;; -*- coding: utf-8; lexical-binding: t; -*-

;;; {{ shell and conf
(my-add-auto-mode 'conf-mode
               "\\.[^b][^a][a-zA-Z]*rc$"
               "\\.aspell\\.en\\.pws$"
               "\\.i3/config-base$"
               "\\mimeapps\\.list$"
               "\\mimeapps\\.list$"
               "\\.editorconfig$"
               "\\.meta$"
               "\\.?muttrc$"
               "\\.mailcap$")
;; }}

;; {{ lisp like language
;; racket
(my-add-auto-mode 'lisp-mode "\\.rkt\\'")
(my-add-auto-mode 'emacs-lisp-mode
               "\\.emacs-project\\'"
               "archive-contents\\'"
               "\\.emacs\\.bmk\\'" )
;; }}

(my-add-auto-mode 'haskell-mode
                  "\\.ghci\\'"
                  "\\.hs\\'")

(my-add-auto-mode 'text-mode
               "TAGS\\'"
               "\\.ctags\\'")

;; vimrc
(my-add-auto-mode 'vimrc-mode "\\.?vim\\(rc\\)?$")

(provide 'init-file-type)
;;; init-file-type.el ends here
