;; -*- coding: utf-8; lexical-binding: t; -*-

;; {{ Write backup files to its own directory
;; @see https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
(defvar my-binary-file-name-regexp
  "\\.\\(avi\\|wav\\|pdf\\|mp[34g]\\|mkv\\|exe\\|3gp\\|rmvb\\|rm\\|pyim\\|\\.recentf\\)$"
  "Is binary file name?")

(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (string-match-p my-binary-file-name-regexp name)))))

(let* ((backup-dir (expand-file-name "~/.backups")))
  (unless (file-exists-p backup-dir) (make-directory backup-dir))
  (setq backup-by-copying t ; don't clobber symlinks
        backup-directory-alist (list (cons "." backup-dir))
        delete-old-versions t
        version-control t  ;use versioned backups
        kept-new-versions 8
        kept-old-versions 4))

;; Donot make backups of files, not safe
;; @see https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq vc-make-backup-files nil)

;; close file backup "~xxx.xx"
(setq make-backup-files nil)
;; }}

;;; {{{ GUI frames
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

(defun my-mini-ui ()
  "Minimum ui."
  ;; NO tool bar, scroll-bar
  (when window-system
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (horizontal-scroll-bar-mode -1)))

(my-run-with-idle-timer 2 #'my-mini-ui)

;; menu-bar is necessery in macos
(unless *mac*
  (menu-bar-mode -1))

;; beacon mode stutup
(defun beacon-per-setup ()
  "beacon setup before load beacon mode."
  ;;(setq-default beacon-color "#3cffcc")
  (setq-default beacon-lighter "")
  (setq-default beacon-size 24))

(add-hook 'beacon-before-blink-hook 'beacon-per-setup)
(my-run-with-idle-timer 2 'beacon-mode)

;; frame title
(defun frame-titile-setup ()
  "Setup format for frame title."
  (setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (file-name-directory buffer-file-name))
                 "%b")))))

(my-run-with-idle-timer 2 'frame-titile-setup)

;; time format
;; If you want to customize time format, read document of `format-time-string'
;; and customize `display-time-format'.
(setq display-time-format "%m-%d %H:%M")

;; from RobinH, Time management
(setq display-time-24hr-format t) ; the date in modeline is English too, magic!
(setq display-time-day-and-date t)
(my-run-with-idle-timer 2 #'display-time)

;; }}}

;; Nicer naming of buffers for files with identical names
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(global-set-key (kbd "M-/") 'hippie-expand)

;; {{ indention management
(defun my-indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-region (point-min) (point-max))
        (message "Indent buffer.")))))
;; }}

(with-eval-after-load 'browse-kill-ring
  (setq browse-kill-ring-separator "--------------------------")
  (define-key browse-kill-ring-mode-map (kbd "q") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "C-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "C-p") 'browse-kill-ring-previous))

;; midnight mode purges buffers which haven't been displayed in 3 days
(my-run-with-idle-timer 4 #'midnight-mode)


;; {{ pomodoro
(with-eval-after-load 'pomodoro
  (setq pomodoro-play-sounds nil) ; *.wav is not installed
  (setq pomodoro-break-time 2)
  (setq pomodoro-long-break-time 5)
  (setq pomodoro-work-time 15)
  ;; Instead of calling `pomodoro-add-to-mode-line`
  (push '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format))
;; }}

(defun my-current-string-beginning ()
  "Goto current string's beginning."
  (interactive)
  (goto-char (car (my-create-range t))))

(defun my-current-string-end ()
  "Goto current string's end."
  (interactive)
  (goto-char (1- (cdr (my-create-range t)))))

;; {{
;; Random line sorting
(defun my-sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))
;; }}

;; {{
(with-eval-after-load 'whitespace
  (setq-default show-trailing-whitespace nil)
  (defun my-show-trailing-whitespace ()
    "Enable display of trailing whitespace in this buffer."
    (setq-local show-trailing-whitespace t))
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook 'my-show-trailing-whitespace)))

(global-set-key [remap just-one-space] 'cycle-spacing)
;; }}

(with-eval-after-load 'symbol-overlay
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

(add-hook 'dired-mode-hook 'org-download-enable)

;; {{
;; Ubuntu 下 Emacs 中无法激活搜狗输入法?
;; 这个帖子估计还会有人遇到类似问题， 我也回复下。
;; 最简单的办法是替换emacs的启动文件，就是那个.desktop文件 在/usr/share/applications/下面。
;; 直接改成Exec=env LC_CTYPE=zh_CN.UTF-8 emacs25 %F
;; 加粗部分是新增的内容。
;; 我看到帖子的内容也尝试去修改自己的profile或者是系统的locale，结果非常坑，也不知道是哪里设置的，
;; 家目录设置的始终不生效，系统级别也跟我得到的结果不一致，再加上我并不希望修改系统的设置，
;; 现在的办法看起来是最完美的了。本来也是emacs的一个bug。。
;; font setup
(defun my-font-setup ()
  (when (window-system)
    ;; set english font
    (set-frame-font "Monospace 15")))

;; set chinses font
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "simsun"))
  (setq face-font-rescale-alist '(("simsun" . 1.1))))

(my-run-with-idle-timer 2 'my-font-setup)
;;}}

(provide 'init-essential)
;;; init-essential.el ends here
