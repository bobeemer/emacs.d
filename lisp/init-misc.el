;; -*- coding: utf-8; lexical-binding: t; -*-

;; open header file under cursor
(global-set-key (kbd "C-x C-o") 'ffap)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; {{ isearch
;; Use regex to search by default, i am prefer macos keys: s-d and s-g
;; (global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-M-r") 'isearch-backward-regexp)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
;; }}

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)

;; @see http://stackoverflow.com/questions/4222183/emacs-how-to-jump-to-function-definition-in-el-file
(global-set-key (kbd "C-h C-f") 'find-function)

;; {{ helpful (https://github.com/Wilfred/helpfu)
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up Commands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

(with-eval-after-load 'counsel
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(global-hl-line-mode t)

;; some project prefer tab, so be it
;; @see http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
(setq-default tab-width 4)

(setq history-delete-duplicates t)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

(setq system-time-locale "C")

(setq imenu-max-item-length 256)

;; {{ recentf-mode
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 2048
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "recentf$"
                        "company-statistics-cache\\.el$"
                        ;; ctags
                        "/TAGS$"
                        ;; global
                        "/GTAGS$"
                        "/GRAGS$"
                        "/GPATH$"
                        ;; binary
                        "\\.mkv$"
                        "\\.mp[34]$"
                        "\\.avi$"
                        "\\.wav$"
                        "\\.docx?$"
                        "\\.xlsx?$"
                        ;; sub-titles
                        "\\.sub$"
                        "\\.srt$"
                        "\\.ass$"
                        ;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
                        ))
;; }}

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Ctrl-X, u/l  to upper/lowercase regions without confirm
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; {{ Answer Yes/No programmically when asked by `y-or-n-p'
(defvar my-default-yes-no-answers nil
  "Usage: (setq my-default-yes-no-answers '((t . \"question1\") (t . \"question2\")))).")
(defun my-y-or-n-p-hack (orig-func &rest args)
  "Answer yes or no automatically for question(ORIG-FUNC ARGS)."
  (let* ((prompt (car args))
         rlt)
    (cond
     ((and my-default-yes-no-answers
           (listp my-default-yes-no-answers))
      (let* ((i 0) found cand)
        (while (and (setq cand (nth i my-default-yes-no-answers))
                    (not found))
          (when (string-match-p (cdr cand) prompt)
            (setq found t)
            (setq rlt (car cand)))
          (setq i (1+ i)))
        (unless found (setq rlt (apply orig-func args)))))
     (t
      (setq rlt (apply orig-func args))))
    rlt))
(advice-add 'y-or-n-p :around #'my-y-or-n-p-hack)
;; }}

;;a no-op function to bind to if you want to set a keystroke to null
(defun void () "This is a no-op." (interactive))

(defalias 'list-buffers 'ibuffer)

(defun add-pwd-into-load-path ()
  "Add current directory into `load-path', useful for elisp developers."
  (interactive)
  (let* ((dir (expand-file-name default-directory)))
    (if (not (memq dir load-path))
        (add-to-list 'load-path dir))
    (message "Directory added into load-path:%s" dir)))

;; {{
(defun my-minibuffer-setup-hook ()
  "Set up mini buffer."
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard)
  (local-set-key (kbd "C-k") 'kill-line)
  (subword-mode 1) ; enable sub-word movement in minibuffer
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Hook when exist mini buffer."
  ;; evil-mode also use mini buffer
  (setq gc-cons-threshold 67108864))

(defun minibuffer-inactive-mode-hook-setup ()
  "Mibibuffer more profermance."
  ;; Make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer.
  ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'.
  (set-syntax-table (let* ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))

;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
(add-hook 'minibuffer-inactive-mode-hook 'minibuffer-inactive-mode-hook-setup)
;; }}

;; {{ xterm
(defun my-run-after-make-frame-hook (frame)
  "Hook after create new FRAME."
  (select-frame frame)
  (unless window-system
    ;; Mouse in a terminal (Use shift to paste with middle button)
    (xterm-mouse-mode 1)))
(add-hook 'after-make-frame-functions 'my-run-after-make-frame-hook)
;; }}

(defun my-toggle-indentation ()
  "Toggle INDENT-TABS-MODE."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode=%s" indent-tabs-mode))

;;; {{ add && remove display long lines in style (end line with $)
(defun my-hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun my-remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(add-hook 'magit-status-mode-hook 'my-hidden-dos-eol)
;; }}

;; @see https://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun kill-all-but-current-buffer ()
  "Kill other buffers."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

;; I'm in Australia now, so I set the locale to "en_AU"
(defun my-insert-date (prefix)
  "Insert the current date with prefix-argument, use ISO format.
With two PREFIX arguments, write out the day and month name."
  (interactive "P")
  (let* ((format (cond
                  ((not prefix) "%d.%m.%Y")
                  ((equal prefix '(4)) "%Y-%m-%d")
                  ((equal prefix '(16)) "%d %B %Y"))))
    (insert (format-time-string format))))

;;compute the length of the marked region
(defun region-length ()
  "Length of a selected region."
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;; show ascii table
(defun ascii-table ()
  "Print the ascii table."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let* ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;; {{ unique lines
;; https://gist.github.com/ramn/796527
;; uniq-lines
(defun uniq-lines (start end)
  "Unique lines(START END)."
  (interactive "*r")
  (delete-duplicate-lines start end))
;; }}

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a BUFFER-SAVE-HOOK, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))
;; {{ some new line function form steve pull work with evil

(defun my-newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (evil-insert 1))

(define-key global-map (kbd "S-<return>") 'my-newline-at-end-of-line)

(defun my-kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (when (paredit-mode 1)
      (paredit-mode -1))
    (back-to-indentation)
    (kill-region (point) prev-pos)
    (unless (paredit-mode -1)
      (paredit-mode 1)))
  (evil-insert 1))

(global-set-key (kbd "C-M-<backspace>") 'my-kill-back-to-indentation)

(defun my-kill-line ()
  "Kill current line and move to top line."
  (interactive)
  (kill-line 1)
  (backward-delete-char 1))

(global-set-key (kbd "C-S-k") 'join-line)

(defun my-open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-l") 'my-open-line-with-reindent)

;;; delete contnet current cursor to character
;; (global-set-key (kbd "M-Z") 'zap-up-to-char)

;;{{ code floder, origami mode add autoload, use this key
;; should be M-x: origami-mode
(with-eval-after-load 'origami
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes))
;; }}

(with-eval-after-load 'move-dup
  ;; move dup key
  (global-set-key [M-up] 'move-dup-move-lines-up)
  (global-set-key [M-down] 'move-dup-move-lines-down)
  ;;(global-set-key [M-up] 'move-dup-move-lines-up)
  ;;(global-set-key [M-down] 'move-dup-move-lines-down)
  (global-set-key [M-S-up] 'move-dup-move-line-or-region))

;; my screen is tiny, so I use minimum eshell prompt
(with-eval-after-load 'eshell
  (setq eshell-prompt-function
        (lambda ()
          (concat (getenv "USER") " $ "))))

;; {{ exec path from shell setup
;; eg: brew list ag, the path can add to PATH,
;; ag can be found by mac emacs
(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (and window-system (memq window-system '(mac ns)))
  ;;@see https://github.com/purcell/exec-path-from-shell/issues/75
  ;;I don't use those exec path anyway.
  (my-run-with-idle-timer 4 #'exec-path-from-shell-initialize))
;; }}

(setq-default buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              line-spacing 0
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              ;; void problems with crontabs, etc.
              ;; require-final-newline t ; bad idea, could accidentally edit others' code
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil)


;; @see http://www.emacswiki.org/emacs/SavePlace
(cond
 ((fboundp 'save-place-mode)
  (save-place-mode 1))
 (t
  (require 'saveplace)
  (setq-default save-place t)))

;; global-auto-revert-mode is a mirror mode that update file when file is changed in otherwise.
(unless (eq system-type 'windows-nt)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (my-run-with-idle-timer 4 #'global-auto-revert-mode))

;; {{ bookmark
;; use my own bookmark if it exists
(with-eval-after-load 'bookmark
  (if (file-exists-p (file-truename "~/.emacs.bmk"))
      (setq bookmark-file (file-truename "~/.emacs.bmk"))))
;; }}

;; {{ avy, jump between texts, like easymotion in vim
;; @see http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/ for more tips
;; dired
(with-eval-after-load 'dired
  (diredfl-global-mode 1)
  (define-key dired-mode-map (kbd ";") 'avy-goto-subword-1))
;; }}

;; {{ auto-save.el
(defun my-check-major-mode-for-auto-save (file)
  "Check current major mode of FILE for auto save."
  (ignore file)
  (memq major-mode my-auto-save-exclude-major-mode-list))

(with-eval-after-load 'auto-save
  (push 'file-remote-p auto-save-exclude)
  (push 'my-file-too-big-p auto-save-exclude)
  (push 'my-check-major-mode-for-auto-save auto-save-exclude)
  (setq auto-save-idle 2) ; 2 seconds
  (setq auto-save-slient t))
(my-run-with-idle-timer 4 #'auto-save-enable)
;; }}

;; {{
(with-eval-after-load 'grep
  ;; eacl and other general grep (rgrep, grep ...) setup
  (dolist (v '("auto"
               "target"
               "node_modules"
               "bower_components"
               "*dist"
               ".sass_cache"
               ".cache"
               ".npm"
               "elpa"))
    (add-to-list 'grep-find-ignored-directories v))
  (dolist (v '("*.min.js"
               "*.map"
               "*.bundle.js"
               "*.min.css"
               "tags"
               "TAGS"
               "GTAGS"
               "GRTAGS"
               "GPATH"
               "cscope.files"
               "*.json"
               "*.log"))
    (add-to-list 'grep-find-ignored-files v))

  ;; wgrep and rgrep, inspired by http://oremacs.com/2015/01/27/my-refactoring-workflow/
  (define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(defun my-wgrep-mark-deletion-hack (&optional arg)
  "After mark a line for deletion, move to next line.
ARG is ignored."
  (ignore arg)
  (forward-line))
(advice-add 'wgrep-mark-deletion :after #'my-wgrep-mark-deletion-hack)

;; wgrep and rgrep, inspired by http://oremacs.com/2015/01/27/my-refactoring-workflow/
(with-eval-after-load 'wgrep
  '(define-key grep-mode-map
     (kbd "C-c C-c") 'wgrep-finish-edit))
;; }}

;; {{ https://www.emacswiki.org/emacs/EmacsSession better than "desktop.el" or "savehist".
;; Any global variable matching `session-globals-regexp' is saved *automatically*.
(setq session-save-file (expand-file-name (concat my-emacs-dir ".session")))
(setq session-globals-max-size 2048)
;; can store 8Mb string
(setq session-globals-max-string (* 8 1024 1024))
(setq session-globals-include '(kill-ring
                                (session-file-alist 100 t)
                                my-dired-commands-history
                                file-name-history
                                search-ring
                                regexp-search-ring))
(setq session-save-file-coding-system 'utf-8)
(add-hook 'after-init-hook 'session-initialize)
;; }}

;; {{ wgrep setup
(with-eval-after-load 'wgrep
  ;; save the change after wgrep finishes the job
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-too-many-file-length 2024))
;; }}

;; {{ which-key-mode
(defvar my-show-which-key-when-press-C-h nil)
(with-eval-after-load 'which-key
  (setq which-key-allow-imprecise-window-fit t) ; performance
  (setq which-key-separator ":")
  (setq which-key-idle-delay 1.5)
  (when my-show-which-key-when-press-C-h
    ;; @see https://twitter.com/bartuka_/status/1327375348959498240?s=20
    ;; Therefore, the which-key pane only appears if I hit C-h explicitly.
    ;; C-c <C-h> for example - by Wanderson Ferreira
    (setq which-key-idle-delay 10000)
    (setq which-key-show-early-on-C-h t))
  (setq which-key-idle-secondary-delay 0.05))
(my-run-with-idle-timer 2 #'which-key-mode)
;; }}

(provide 'init-misc)
;;; init-misc.el ends here
