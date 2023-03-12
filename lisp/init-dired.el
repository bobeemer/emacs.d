;; -*- coding: utf-8; lexical-binding: t; -*-

(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
If no files marked, always operate on current line in dired-mode."
  (interactive
   (let* ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))

(defun dired-mode-hook-setup ()
  "Set up dired."
  (dired-hide-details-mode 1)
  (local-set-key  "r" 'dired-up-directory)
  (local-set-key  "e" 'my-ediff-files)
  (local-set-key  "/" 'dired-isearch-filenames)
  (local-set-key  "\\" 'diredext-exec-git-command-in-shell))
(add-hook 'dired-mode-hook 'dired-mode-hook-setup)

;; https://www.emacswiki.org/emacs/EmacsSession which is easier to use
;; See `session-globals-regexp'
;; If the variable is named like "*-history", it will be *automatically* saved.
(defvar my-dired-directory-history nil
  "Recent directories accessed by dired.")

(defvar my-dired-exclude-directory-regexp nil
  "Dired directories matching this regexp are not added into directory history.")

(defun my-shell-directories-from-fasd ()
  "Directories from fasd (https://github.com/clvv/fasd) in shell."
  (and (executable-find "fasd")
       (my-nonempty-lines (shell-command-to-string "fasd -ld"))))

(defun my-shell-directories-from-z ()
  "Directories from z (https://github.com/rupa/z) in shell."
  (mapcar #'car (shellcop-directories-from-z)))

(defvar my-shell-directory-history-function #'my-shell-directories-from-fasd
  "Return directory history in shell.  Used by `my-recent-directory'.")

(defun my-recent-directory (&optional n)
  "Goto recent directories.
If N is not nil, only list directories in current project."
  (interactive "P")
  (unless recentf-mode (recentf-mode 1))
  (let* ((cands (delete-dups
                 (append my-dired-directory-history
                         (mapcar 'file-name-directory recentf-list)
                         (and my-shell-directory-history-function
                              (funcall my-shell-directory-history-function)))))
         (root-dir (if (ffip-project-root) (file-truename (ffip-project-root)))))

    (when (and n root-dir)
      ;; return directories in project root
      (setq cands
            (cl-remove-if-not (lambda (f) (path-in-directory-p f root-dir)) cands)))

    (when my-dired-exclude-directory-regexp
      (setq cands
            (cl-remove-if (lambda (f) (string-match my-dired-exclude-directory-regexp f))
                          cands)))

    (dired (completing-read "Directories: " cands))))

(with-eval-after-load 'dired
  ;; re-use dired buffer, available in Emacs 28
  ;; @see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20598
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)

  ;; when there is two dired buffer, Emacs will select another buffer
  ;; as target buffer (target for copying files, for example).
  ;; It's similar to windows commander.
  (setq dired-dwim-target t)

  ;; Listing directory failed but access-file worked
  (when *mac*
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))

  ;; @see http://blog.twonegatives.com/post/19292622546/dired-dwim-target-is-j00-j00-magic
  ;; op open two new dired buffers side-by-side and give your new-found automagic power a whirl.
  ;; Now combine that with a nice window configuration stored in a register and you’ve got a pretty slick work flow.
  (setq dired-dwim-target t)

  (my-ensure 'dired-x)
  (my-ensure 'dired-aux) ; for `dired-dwim-target-directory'

  ;; @see https://emacs.stackexchange.com/questions/5649/sort-file-names-numbered-in-dired/5650#5650
  (setq dired-listing-switches "-laGh1v")
  (setq dired-recursive-deletes 'always))

(provide 'init-dired)
;;; init-dired.el ends here
