;; -*- coding: utf-8; lexical-binding: t; -*-

(my-run-with-idle-timer 1 #'ivy-mode) ; it enables ivy UI for `kill-buffer'

(with-eval-after-load 'ivy
  ;; better performance on everything (especially windows), ivy-0.10.0 required
  ;; when `ivy-dynamic-exhibit-delay-ms' is a non-zero value
  ;; Setting it to a bigger value in ALL OSs is also more green energy btw.
  ;; @see https.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 250)

  ;; Press C-p and Enter to select current input as candidate
  ;; https://oremacs.com/2017/11/30/ivy-0.10.0/
  (setq ivy-use-selectable-prompt t)

  ;; M-x show describe, should use melpa version instead of melpa-stable
  (ivy-rich-mode)
  (setq enable-recursive-minibuffers t)

   ;; @see https://oremacs.com/2015/07/23/ivy-multiaction/
  ;; press "M-o" to choose ivy action after run command counsel-find-file.
  (ivy-set-actions
   'counsel-find-file
   '(("j" find-file-other-frame "other frame")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("x" counsel-find-file-extern "open externally")
     ("d" delete-file "delete")
     ("r" counsel-find-file-as-root "open as root")))

  ;; work around ivy issue.
  ;; @see https://github.com/abo-abo/swiper/issues/828
  (setq ivy-display-style 'fancy))

(defun my-pinyinlib-build-regexp-string (str)
  "Build pinyin regexp from STR."
  (my-ensure 'pinyinlib)
  (let* (rlt (i 0) ch)
    (while (< i (length str))
      (setq ch (elt str i))
      (setq rlt (concat rlt
                        (cond
                         ((and (<= ?a ch) (<= ch ?z))
                          (pinyinlib-build-regexp-char ch))
                         (t
                          (char-to-string ch)))))
      (setq i (1+ i)))
    rlt))

(defun ivy-switch-buffer-matcher-pinyin (regexp candidates)
  (ivy--switch-buffer-matcher (my-pinyinlib-build-regexp-string regexp) candidates))

(defun ivy-switch-buffer-by-pinyin ()
  "Switch to another buffer."
  (interactive)
  (my-ensure 'ivy)
  (let* ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: " 'internal-complete-buffer
              :matcher #'ivy-switch-buffer-matcher-pinyin
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'ivy-switch-buffer)))

;; {{ swiper&ivy-mode
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
;; }}

;;(setq ivy-use-virtual-buffers t) ; not good experience
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; use helpful pkg instead.
;; (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)
;; (global-set-key (kbd "C-h f") 'counsel-describe-function)


(defun ivy-occur-grep-mode-hook-setup ()
  "Set up ivy occur grep mode."
  ;; no syntax highlight, I only care performance when searching/replacing
  (font-lock-mode -1)
  ;; @see https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
  (column-number-mode -1)
  ;; turn on wgrep right now
  ;; (ivy-wgrep-change-to-wgrep-mode) ; doesn't work, don't know why
  (local-set-key (kbd "RET") #'ivy-occur-press-and-switch))
(add-hook 'ivy-occur-grep-mode-hook 'ivy-occur-grep-mode-hook-setup)


(provide 'init-ivy)
;;; init-ivy.el ends here
