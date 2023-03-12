;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(setq *mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))

;; don't GC during startup to save time
(unless (bound-and-true-p my-computer-has-smaller-memory-p)
  (setq gc-cons-percentage 0.6)
  (setq gc-cons-threshold most-positive-fixnum))

(setq *no-memory* (cond
                   (*mac*
                    ;; @see https://discussions.apple.com/thread/1753088
                    ;; "sysctl -n hw.physmem" does not work
                    (<= (string-to-number (shell-command-to-string "sysctl -n hw.memsize"))
                        (* 4 1024 1024)))
                   (*linux* nil)
                   (t nil)))

(defconst my-emacs-dir (file-name-as-directory user-emacs-directory)
  "Directory of .emacs.d.")

(defconst my-lisp-dir (concat my-emacs-dir "lisp")
  "Directory of lisp.")

(defconst my-site-lisp-dir (concat my-emacs-dir "site-lisp")
  "Directory of site-lisp.")

(defun require-init (pkg &optional maybe-disabled)
  "Load Installed PKG."
    (load (file-truename (format "%s/%s" my-lisp-dir pkg)) t t))

(defun my-add-subdirs-to-load-path (lisp-dir)
  "Add sub-directories under LISP-DIR into `load-path'."
  (let* ((default-directory lisp-dir))
    (setq load-path
          (append
           (delq nil
                 (mapcar (lambda (dir)
                           (unless (string-match-p "^\\." dir)
                             (expand-file-name dir)))
                         (directory-files my-site-lisp-dir)))
           load-path))))

(let* ((file-name-handler-alist nil))
  
  (require-init 'init-autoload)
  (require-init 'init-modeline)
  (require-init 'init-utils)
  (require-init 'init-file-type)
  (require-init 'init-elpa)

  (when my-disable-idle-timer
    (my-add-subdirs-to-load-path (file-name-as-directory my-site-lisp-dir)))

  (require-init 'init-windows)
  (require-init 'init-linum-mode)
  (require-init 'init-ibuffer t)
  (require-init 'init-company)
  (require-init 'init-lisp t)
  (require-init 'init-python t)
  (require-init 'init-ivy)
  ;;(require-init 'init-clipboard)
  (require-init 'init-theme)
  ;; crucial tools I need immediately
  (require-init 'init-misc t)
  ;; handy tools though not must have
  (require-init 'init-essential)
  (require-init 'init-org t)
  (require-init 'init-dired)
  (require-init 'init-yasnippet)
  (require-init 'init-keyfreq)
  (require-init 'init-term-mode)
  (require-init 'init-workgroup)
  (require-init 'init-hydra)
  (require-init 'init-shackle t)
  (require-init 'init-haskell)
  (require-init 'init-cpp-mode)
  (require-init 'init-evil)

  (unless my-disable-idle-timer
    (my-add-subdirs-to-load-path (file-name-as-directory my-site-lisp-dir)))

  (setq custom-file (expand-file-name (concat my-emacs-dir "custom-set-variables.el")))
  (if (file-exists-p custom-file) (load custom-file t t))
  (load (expand-file-name "~/.custom.el") t nil))

;; set windos gc
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(run-with-idle-timer 4 nil #'my-cleanup-gc)

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
